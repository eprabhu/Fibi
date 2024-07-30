
DROP VIEW IF EXISTS coi_project_institute_proposal_v;
DROP VIEW IF EXISTS coi_project_proposal_v;
DROP VIEW IF EXISTS coi_project_award_v;

\. ./Views/coi_project_institute_proposal_v.sql
\. ./Views/coi_project_proposal_v.sql
\. ./Views/coi_project_award_v.sql

