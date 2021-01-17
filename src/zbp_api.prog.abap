*&---------------------------------------------------------------------*
*& Report ZBP_API
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbp_api.

SELECTION-SCREEN BEGIN OF BLOCK bl0 WITH FRAME TITLE TEXT-t00.
  SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-t01.
    PARAMETERS: p_unam TYPE string LOWER CASE OBLIGATORY,
                p_pass TYPE string LOWER CASE OBLIGATORY,
                p_url  TYPE string LOWER CASE OBLIGATORY.
  SELECTION-SCREEN END OF BLOCK bl1.
SELECTION-SCREEN END OF BLOCK Bl0.

AT SELECTION-SCREEN OUTPUT.
  "have a little decency and hide password field
  LOOP AT SCREEN.
    IF screen-name = 'P_PASS'.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.

CLASS lcl_odata_tool DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_csrf_token_and_cookie
      EXPORTING
        !et_cookies TYPE tihttpcki
        !ev_token   TYPE string .

    CLASS-METHODS create_opp
      IMPORTING
        !iv_token   TYPE string
        !it_cookies TYPE tihttpcki .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS lCL_ODATA_TOOL IMPLEMENTATION.
  METHOD get_csrf_token_and_cookie.
    DATA: lo_http_client TYPE REF TO if_http_client,
          lv_status      TYPE i,
          lt_fields      TYPE tihttpnvp,
          lv_sysubrc     TYPE sysubrc.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = p_url
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    ASSERT sy-subrc = 0.
    lo_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.

    CALL METHOD lo_http_client->request->set_method( if_http_request=>co_request_method_get ).

    lo_http_client->request->set_header_field( name = if_rest_request=>gc_header_csrf_token value = 'Fetch' ).
    lo_http_client->request->set_header_field( name = 'Accept' value = if_rest_media_type=>gc_appl_json ).
    lo_http_client->request->set_header_field( name = 'Content-Type' value = if_rest_media_type=>gc_appl_json ).
    lo_http_client->request->set_header_field( name = if_http_form_fields_sap=>sap_client value = '100' ).

    lo_http_client->request->set_authorization( auth_type  = ihttp_auth_type_basic_auth
                                                username   = p_unam
                                                password   = p_pass ).

    lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

    CALL METHOD lo_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    ASSERT sy-subrc = 0.

    CALL METHOD lo_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    IF sy-subrc <> 0.
      CALL METHOD lo_http_client->get_last_error
        IMPORTING
          code    = lv_sysubrc
          message = DATA(ev_message).
      WRITE: / 'Error when getting token:', ev_message.
      RETURN.
    ENDIF.

    lo_http_client->response->get_header_fields( CHANGING fields = lt_fields ).
    READ TABLE lt_fields ASSIGNING FIELD-SYMBOL(<field>) WITH KEY name = 'x-csrf-token'.
    ev_token = <field>-value.

    lo_http_client->response->get_cookies( CHANGING cookies = et_cookies ).
    lo_http_client->close( ).

  ENDMETHOD.

  METHOD create_opp.

    DATA:lo_http_client TYPE REF TO if_http_client,
         lv_sysubrc     TYPE sysubrc,
         lv_body        TYPE string,
         ls_bp          TYPE zapi_s_bsnsp,
         lt_adress      TYPE TABLE OF zapi_s_adress,
         lr_out         TYPE REF TO data.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = p_url
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    ASSERT sy-subrc = 0.
    lo_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.

    CALL METHOD lo_http_client->request->set_method( if_http_request=>co_request_method_post ).

    lo_http_client->request->set_header_field( name = 'Content-Type' value = if_rest_media_type=>gc_appl_json ).
    lo_http_client->request->set_header_field( name = 'Accept' value = if_rest_media_type=>gc_appl_json ).
    lo_http_client->request->set_header_field( name = 'x-csrf-token' value = iv_token ).

    lo_http_client->request->set_authorization( auth_type  = ihttp_auth_type_basic_auth
                                                username   = p_unam
                                                password   = p_pass ).

    lo_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client value = '100' ).


    LOOP AT it_cookies ASSIGNING FIELD-SYMBOL(<cookie>).
      lo_http_client->request->set_cookie( name = <cookie>-name
                                           value = <cookie>-value ).
    ENDLOOP.

    lt_adress = VALUE #( ( _country     = 'TR'     _street_name       = 'Street Name'
                           _postal_code = '69190'  _city_name         = 'İstanbul'
                           _language    = 'TR'     _address_time_zone = 'TURKEY'
                           _full_name   = 'Hasan Tugay'
                          to__address_usage = VALUE #( ( _address_usage = 'XXDEFAULT' ) ) )
                        ( _country      = 'TR'     _street_name       = 'Street Name'
                          _postal_code  = '69190'  _city_name         = 'Kırşehir'
                          _language     = 'TR'     _address_time_zone = 'TURKEY'
                          _full_name    = 'Hasan Tugay'
                           to__address_usage = VALUE #( ( _address_usage = 'SHIP_TO' ) ) ) ).

    ls_bp = VALUE #( _business_partner_category     = '2'
                     _organization_b_p_name1        = 'Tugay'
                     _search_term1                  = '12345'
                     _language                      = 'TR'
                     _correspondence_language       = 'TR'
                     _business_partner_full_name    = 'Hasan Tugay'
                     _business_partner_grouping     = 'M001'
                     _business_partner_name         = 'Hasan Tugay'
                     to__business_partner_address   = lt_adress
                     to__business_partner_tax       = VALUE #( ( _b_p_tax_type   = 'DE0'  _b_p_tax_number = 'DE012345678' ) )
                     to__business_partner_role      = VALUE #( ( _business_partner_role = 'FLCU00') ( _business_partner_role = 'FLCU01' ) )
                     to__customer                   = VALUE #( _customer_account_group = 'M001'
                                                               _customer_full_name     = 'Hasan Tugay Birihan/12000 DFGFG'
                                                               _customer_name          = 'Hasan Tugay Birihan'
                                                               ) ).


    lv_body = /ui2/cl_json=>serialize( data = ls_bp
                                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                       assoc_arrays = abap_true assoc_arrays_opt = abap_true ).

    REPLACE ALL OCCURRENCES OF 'BusinessPartneridbyextsystem' IN lv_body WITH `BusinessPartnerIDByExtSystem`.
    REPLACE ALL OCCURRENCES OF 'InternationalLocationnumber1' IN lv_body WITH `InternationalLocationNumber1`.
    REPLACE ALL OCCURRENCES OF 'InternationalLocationnumber2' IN lv_body WITH `InternationalLocationNumber2`.
    REPLACE ALL OCCURRENCES OF 'InternationalLocationnumber3' IN lv_body WITH `InternationalLocationNumber3`.
    REPLACE ALL OCCURRENCES OF 'AddressIdByExternalSystem'    IN lv_body WITH `AddressIDByExternalSystem`.
    REPLACE ALL OCCURRENCES OF 'to_addressUsage'              IN lv_body WITH `to_AddressUsage`.
    REPLACE ALL OCCURRENCES OF 'to_businessPartnerAddress'    IN lv_body WITH `to_BusinessPartnerAddress`.
    REPLACE ALL OCCURRENCES OF 'to_businessPartnerTax'        IN lv_body WITH `to_BusinessPartnerTax`.
    REPLACE ALL OCCURRENCES OF 'to_businessPartnerRole'       IN lv_body WITH `to_BusinessPartnerRole`.
    REPLACE ALL OCCURRENCES OF 'to_customer'                  IN lv_body WITH `to_Customer`.
    REPLACE ALL OCCURRENCES OF 'BillingisblockedForCustomer'  IN lv_body WITH `BillingIsBlockedForCustomer`.

    lo_http_client->request->set_cdata( data = lv_body ).
    CALL METHOD lo_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    ASSERT sy-subrc = 0.

    CALL METHOD lo_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    IF sy-subrc <> 0.
      CALL METHOD lo_http_client->get_last_error
        IMPORTING
          code    = lv_sysubrc
          message = DATA(ev_message).
      WRITE: / 'error occurred during receive data' COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.
    DATA(lv_json) = lo_http_client->response->get_cdata( ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_json pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                CHANGING data = lr_out ).

    cl_demo_output=>write_json( lv_json ).
    cl_demo_output=>display( ).


  ENDMETHOD.


ENDCLASS.

START-OF-SELECTION.
  DATA(lo_class) = NEW lcl_odata_tool( ).

  lo_class->get_csrf_token_and_cookie(
    IMPORTING
      et_cookies = DATA(et_cookies)
      ev_token   = DATA(ev_token)
  ).

  lo_class->create_opp(
    EXPORTING
      iv_token   = ev_token
      it_cookies = et_cookies
  ).
