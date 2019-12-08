METHOD load_all_data.
* data declarations

  DATA: lo_http_client     TYPE REF TO if_http_client,
        lo_rest_client     TYPE REF TO cl_rest_http_client,
        lv_url             TYPE string VALUE '/erp/registro',
        http_status        TYPE string,
        lv_body            TYPE string.

  DATA:
        lo_response        TYPE REF TO if_rest_entity,
        t_data             TYPE ztthr_fhtt001,
        t_response         TYPE TABLE OF zshr_fhtt001,
        wa_response        LIKE LINE OF t_response,
        wa_data            LIKE LINE OF t_data,
        reason             TYPE  string,
        response           TYPE  string,
        content_length     TYPE  string,
        location           TYPE  string,
        content_type       TYPE  string,
        json_response      TYPE  string,
        oref               TYPE REF TO cx_root,
        ld_subrc           TYPE sy-subrc,
        it_infty_p0465     TYPE TABLE OF p0465,
        wa_infty_p0465     LIKE LINE OF it_infty_p0465.

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

  CREATE OBJECT lo_rest_client
    EXPORTING
      io_http_client = lo_http_client.

  lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).

  IF lo_http_client IS BOUND AND lo_rest_client IS BOUND.
    cl_http_utility=>set_request_uri(
      EXPORTING
        request = lo_http_client->request    " HTTP Framework (iHTTP) HTTP Request
        uri     = lv_url                     " URI String (in the Form of /path?query-string)
    ).

* Set headers
    CALL METHOD lo_rest_client->if_rest_client~set_request_header
      EXPORTING
        iv_name  = 'Content-type'
        iv_value = 'application/json'.

* GET
    TRY.
        lo_rest_client->if_rest_resource~get( ).
      CATCH  cx_rest_client_exception INTO oref.
        lo_response     = lo_rest_client->if_rest_client~get_response_entity( ).
        http_status     = lo_response->get_header_field( '~status_code' ).
        reason          = lo_response->get_header_field( '~status_reason' ).
        RAISE connection_failed.
    ENDTRY.

* Collect response
    lo_response    = lo_rest_client->if_rest_client~get_response_entity( ).
    http_status    = lo_response->get_header_field( '~status_code' ).
    reason         = lo_response->get_header_field( '~status_reason' ).
    content_length = lo_response->get_header_field( 'content-length' ).
    location       = lo_response->get_header_field( 'location' ).
    content_type   = lo_response->get_header_field( 'content-type' ).
    response       = lo_response->get_string_data( ).

* HTTP JSON return string
    json_response = lo_response->get_string_data( ).

* Class to convert the JSON to an ABAP sttructure
    /ui2/cl_json=>deserialize( EXPORTING json = json_response CHANGING data =  t_response ).

    LOOP AT t_response INTO wa_response.

** PIS
*      CALL FUNCTION 'HR_READ_INFOTYPE'
*        EXPORTING
*          tclas           = 'A'
*          pernr           = wa_response-matricula
*          infty           = '0465'
*          begda           = sy-datum
*          endda           = sy-datum
*        IMPORTING
*          subrc           = ld_subrc
*        TABLES
*          infty_tab       = it_infty_p0465
*        EXCEPTIONS
*          infty_not_found = 1
*          OTHERS          = 2.
*
*      IF it_infty_p0465 IS NOT INITIAL.
*        SORT it_infty_p0465 BY subty.
*        READ TABLE it_infty_p0465 INTO wa_infty_p0465 WITH KEY subty = '0006' BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*          wa_response-pis =  wa_infty_p0465-pis_nr.
*        ELSE.
*          CONTINUE.
*        ENDIF.
*      ELSE.
*        CONTINUE.
*      ENDIF.

      SHIFT wa_response-pis LEFT DELETING LEADING '0'.
      wa_response-data = wa_response-data+4(4) && wa_response-data+2(2) && wa_response-data(2).
      wa_response-tipo = '3'.
      wa_response-terminal = 'X'.
      wa_response-mensagem = 'Dado importado REP - SAP (endpoint)'.


      MOVE-CORRESPONDING wa_response TO wa_data.
      APPEND wa_data TO t_data.

    ENDLOOP.

    t_zfhtt0001 = t_data.


  ENDIF.


ENDMETHOD.