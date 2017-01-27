REPORT zgantt_test.

PARAMETERS: p_cproj  TYPE c RADIOBUTTON GROUP g1 DEFAULT 'X',
            p_pdescr TYPE zgantt_projects-description.

PARAMETERS: p_ctask TYPE c RADIOBUTTON GROUP g1,
            p_tpro  TYPE zgantt_tasks-project_id,
            p_tdes  TYPE zgantt_tasks-description,
            p_test  TYPE zgantt_tasks-estimate,
            p_tpla  TYPE zgantt_tasks-planned_start,
            p_tres  TYPE zgantt_tasks-resource_name.

START-OF-SELECTION.
  PERFORM run.

FORM run.

  CASE abap_true.
    WHEN p_cproj.
      PERFORM create_project.
    WHEN p_ctask.
      PERFORM create_task.
    WHEN OTHERS.
      ASSERT 0 = 1.
  ENDCASE.

ENDFORM.

FORM create_project.

  WRITE: / zcl_gantt_project=>create( VALUE #( description = p_pdescr ) ).

ENDFORM.

FORM create_task.

  WRITE: / zcl_gantt_task=>create( VALUE #(
    project_id = p_tpro
    description = p_tdes
    estimate = p_test
    planned_start = p_tpla
    resource_name = p_tres ) ).

ENDFORM.
