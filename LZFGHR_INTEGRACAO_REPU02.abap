FUNCTION zfnhr_int_rep_update.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_INSERT_REP) TYPE  ZSHR_INSERT_REP OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_HTTP_STATUS) TYPE  STRING
*"     REFERENCE(E_REASON) TYPE  STRING
*"  EXCEPTIONS
*"      CONNECTION_FAILED
*"----------------------------------------------------------------------

* data declarations

  DATA: lo_http_client     TYPE REF TO if_http_client,
        lo_rest_client     TYPE REF TO cl_rest_http_client,
        lv_url             TYPE string  VALUE '/erp/empregado',
        http_status        TYPE string,
        lv_body            TYPE string,
        json_req           TYPE zshr_insert_rep,
        lr_json_serializer TYPE REF TO cl_trex_json_serializer.

  DATA: lo_json            TYPE REF TO cl_clb_parse_json,
        lo_response        TYPE REF TO if_rest_entity,
        lo_request         TYPE REF TO if_rest_entity,
        lo_sql             TYPE REF TO cx_sy_open_sql_db,
        oref               TYPE REF TO cx_root,
        service_return     TYPE zshr_service_response,
        wa_zhrlog_int_rep  TYPE zhrlog_int_rep ,
        response           TYPE string.

  CONSTANTS:
      co_timeout           TYPE string VALUE ` (Timeout)`.


  cl_http_client=>create_by_destination(
   EXPORTING
     destination              = 'PORTAL_REP'      " Logical destination (specified in function call)
   IMPORTING
     client                   = lo_http_client    " HTTP Client Abstraction
   EXCEPTIONS
     argument_not_found       = 1
     destination_not_found    = 2
     destination_no_authority = 3
     plugin_not_active        = 4
     internal_error           = 5
     OTHERS                   = 6
 ).

* HTTP basic authenication
  lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.

** Only for basic validations - MMULDER (This must be erased) - BEGIN
*
*  CALL METHOD cl_http_client=>create_by_url(
*    EXPORTING
*      url                = lv_service
*    IMPORTING
*      client             = lo_http_client
*    EXCEPTIONS
*      argument_not_found = 1
*      plugin_not_active  = 2
*      internal_error     = 3
*      OTHERS             = 4 ).
*
*
*  l_username = 'serviceadmin'.
*  l_password = 'admin123'.
*
*  CALL METHOD lo_http_client->authenticate
*    EXPORTING
*      username = l_username
*      password = l_password.
*
** Only for basic validations - MMULDER (This must be erased) - END

  CREATE OBJECT lo_rest_client
    EXPORTING
      io_http_client = lo_http_client.


  lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).


  IF lo_http_client IS BOUND AND lo_rest_client IS BOUND.
*    .
    cl_http_utility=>set_request_uri(
      EXPORTING
        request = lo_http_client->request    " HTTP Framework (iHTTP) HTTP Request
        uri     = lv_url                     " URI String (in the Form of /path?query-string)
    ).

* ABAP to JSON
    MOVE-CORRESPONDING i_insert_rep TO json_req.

* Converted JSON should look like this
*   lv_body = '{ "salesorder":"25252525", "type":"Direct"}'.
    lv_body = /ui2/cl_json=>serialize( data = json_req ).


* Set Payload or body ( JSON or XML)
    lo_request = lo_rest_client->if_rest_client~create_request_entity( ).

*   lo_request->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    TRANSLATE lv_body TO LOWER CASE.
    lo_request->set_string_data( lv_body ).


* Set headers
    CALL METHOD lo_rest_client->if_rest_client~set_request_header
      EXPORTING
        iv_name  = 'Content-type'
        iv_value = 'application/json'.

* POST
    TRY.

        lo_rest_client->if_rest_resource~put( lo_request ).

      CATCH  cx_rest_client_exception INTO oref.

        lo_response     = lo_rest_client->if_rest_client~get_response_entity( ).
        e_http_status   = lo_response->get_header_field( '~status_code' ).
        e_reason        = lo_response->get_header_field( '~status_reason' ).

        IF e_reason IS NOT INITIAL.

          wa_zhrlog_int_rep-matricula = i_insert_rep-matricula.
          wa_zhrlog_int_rep-acao = 'U'.
          wa_zhrlog_int_rep-data = sy-datum.
          wa_zhrlog_int_rep-hora = sy-uzeit.
          wa_zhrlog_int_rep-http_status = e_http_status.
          wa_zhrlog_int_rep-resposta = e_reason && co_timeout.

          MODIFY zhrlog_int_rep FROM wa_zhrlog_int_rep.

        ENDIF.

        RAISE connection_failed.

    ENDTRY.

* Collect response
    lo_response    = lo_rest_client->if_rest_client~get_response_entity( ).
    e_http_status  = lo_response->get_header_field( '~status_code' ).
    e_reason       = lo_response->get_header_field( '~status_reason' ).
    response       = lo_response->get_string_data( ).

    TRY.
        IF response IS NOT INITIAL.
          /ui2/cl_json=>deserialize( EXPORTING json = response CHANGING data =  service_return ).
        ENDIF.
      CATCH cx_root INTO oref .
    ENDTRY.

    IF e_reason IS NOT INITIAL.

      wa_zhrlog_int_rep-matricula = i_insert_rep-matricula.
      wa_zhrlog_int_rep-acao = 'U'.
      wa_zhrlog_int_rep-data = sy-datum.
      wa_zhrlog_int_rep-hora = sy-uzeit.
      wa_zhrlog_int_rep-http_status = e_http_status.

      IF e_reason EQ 'OK'.
        wa_zhrlog_int_rep-resposta = text-003.
      ELSE.
        IF service_return-message IS INITIAL.
          wa_zhrlog_int_rep-resposta = e_reason.
        ELSE.
          wa_zhrlog_int_rep-resposta = service_return-message.
        ENDIF.
      ENDIF.

      MODIFY zhrlog_int_rep FROM wa_zhrlog_int_rep.

    ENDIF.

  ENDIF.
ENDFUNCTION.