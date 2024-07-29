
DROP VIEW IF EXISTS coi_project_proposal_v;
DROP VIEW IF EXISTS coi_project_proposal_status_v;
DROP VIEW IF EXISTS coi_project_proposal_qnr_ans_v;

\. ./Views/coi_project_proposal_v.sql
\. ./Views/coi_project_proposal_status_v.sql
\. ./Views/coi_project_proposal_qnr_ans_v.sql
