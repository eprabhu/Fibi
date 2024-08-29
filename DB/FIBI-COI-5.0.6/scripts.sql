
UPDATE dyn_section_config SET DESCRIPTION = 'Entity Overview' WHERE (SECTION_CODE = 'GE2601');

INSERT INTO `DISCLOSURE_ACTION_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`)
VALUES ('31', 'Disclosure Synced', 'Disclosure Synced', now(), 'quickstart');