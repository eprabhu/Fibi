const BASIC_DETAILS =  {sectionName: 'Basic Details', sectionId: 'EO101'};
const COMPANY_DETAILS = {sectionName: 'Company Details', sectionId: 'EO102'};
const ENTITY_RISK = {sectionName: 'Entity Risk', sectionId: 'EO103'};
const OTHER_REFERENCE_IDS = {sectionName: 'Other Reference IDs', sectionId: 'EO104'};
const ADDITIONAL_INFORMATION =  {sectionName: 'Additional Information', sectionId: 'EO105'};
const ENTITY_ATTACHMENTS = {sectionName: 'Entity Attachments', sectionId: 'EO106'}

const SPONSOR_DETAILS =  {sectionName: 'Sponsor Details', sectionId: 'ES201'};
const SPONSOR_RISK =  {sectionName: 'Sponsor Risk', sectionId: 'ES202'};
const SPONSOR_ATTACHMENTS = {sectionName: 'Sponsor Attachments', sectionId: 'ES203'};
export const SPONSOR_QUESTIONNAIRE_SECTION_ID = 'ES204';
export const SPONSOR_QUESTIONNAIRE_SUB_SECTION_ID = 2624;
const SPONSOR_QUESTIONNAIRE = {sectionName: 'Sponsor Questionnaire',
    sectionId: SPONSOR_QUESTIONNAIRE_SECTION_ID, subSectionId: SPONSOR_QUESTIONNAIRE_SUB_SECTION_ID};

const SUB_AWARD_ORGANISATION = {sectionName: 'Sub-award Organization Details', sectionId: 'ES301'};
const SUB_AWARD_RISK = {sectionName: 'Sub-award Organization Risk', sectionId: 'ES302'};
const SUB_AWARD_ATTACHMENTS = {sectionName: 'Sub-award Organization Attachments', sectionId: 'ES303'};
export const SUB_AWARD_QUESTIONNAIRE_SECTION_ID = 'ES304';
export const SUB_AWARD_QUESTIONNAIRE_SUB_SECTION_ID = 2625;
const SUB_AWARD_QUESTIONNAIRE = {sectionName: 'Sub-award Organization Questionnaire',
    sectionId: SUB_AWARD_QUESTIONNAIRE_SECTION_ID, subSectionId: SUB_AWARD_QUESTIONNAIRE_SUB_SECTION_ID};

const COMPLIANCE_RISK = {sectionName: 'Compliance Risk', sectionId: 'EC401'};
const COMPLIANCE_ATTACHMENTS = {sectionName: 'Compliance Attachments', sectionId: 'EC402'};
export const COMPLIANCE_QUESTIONNAIRE_SECTION_ID = 'EC403';
export const COMPLIANCE_QUESTIONNAIRE_SUB_SECTION_ID = 2626;
const COMPLIANCE_QUESTIONNAIRE = {sectionName: 'Compliance Questionnaire',
    sectionId: COMPLIANCE_QUESTIONNAIRE_SECTION_ID, subSectionId: COMPLIANCE_QUESTIONNAIRE_SUB_SECTION_ID};

const ATT_ENTITY_ATTACHMENTS = {sectionName: 'Entity Attachments', sectionId: 'EA501'};
const ATT_SPONSOR_ATTACHMENTS = {sectionName: 'Sponsor Attachments', sectionId: 'EA502'};
const ATT_ORGANISATION_ATTACHMENTS = {sectionName: 'Organization Attachments', sectionId: 'EA503'};
const ATT_COMPLIANCE_ATTACHMENTS = {sectionName: 'Compliance Attachments', sectionId: 'EA504'};

export const OverviewTabSection = new Map([
    ['BASIC_DETAILS' , BASIC_DETAILS],
    ['COMPANY_DETAILS' , COMPANY_DETAILS],
    ['ENTITY_RISK' , ENTITY_RISK],
    ['OTHER_REFERENCE_IDS' , OTHER_REFERENCE_IDS],
    ['ENTITY_ATTACHMENTS' , ENTITY_ATTACHMENTS]
    // ['ADDITIONAL_INFORMATION' , ADDITIONAL_INFORMATION],
]);

export const SponsorTabSection = new Map([
    ['SPONSOR_DETAILS' , SPONSOR_DETAILS],
    ['SPONSOR_RISK' , SPONSOR_RISK],
    ['SPONSOR_ATTACHMENTS' , SPONSOR_ATTACHMENTS],
    ['SPONSOR_QUESTIONNAIRE' , SPONSOR_QUESTIONNAIRE]
]);

export const SubawardOrganisationTab = new Map([
    ['SUB_AWARD_ORGANISATION' , SUB_AWARD_ORGANISATION],
    ['SUB_AWARD_RISK' , SUB_AWARD_RISK],
    ['SUB_AWARD_ATTACHMENTS' , SUB_AWARD_ATTACHMENTS],
    ['SUB_AWARD_QUESTIONNAIRE' , SUB_AWARD_QUESTIONNAIRE],
    // ['Additional_Information' , ADDITIONAL_INFORMATION],
]);

export const ComplianceTab = new Map([
    ['COMPLIANCE_RISK' , COMPLIANCE_RISK],
    ['COMPLIANCE_ATTACHMENTS' , COMPLIANCE_ATTACHMENTS],
    ['COMPLIANCE_QUESTIONNAIRE' , COMPLIANCE_QUESTIONNAIRE]
]);

export const AttachmentTab = new Map([
    ['ATT_ENTITY_ATTACHMENTS' , ATT_ENTITY_ATTACHMENTS],
    ['ATT_SPONSOR_ATTACHMENTS' , ATT_SPONSOR_ATTACHMENTS],
    ['ATT_ORGANISATION_ATTACHMENTS' , ATT_ORGANISATION_ATTACHMENTS],
    ['ATT_COMPLIANCE_ATTACHMENTS' , ATT_COMPLIANCE_ATTACHMENTS]
]);

export const AUTO_SAVE_DEBOUNCE_TIME = 750;

export const DUPLICATE_MARK_CONFIRMATION_TEXT = 'I confirm that the duplicates are not exact matches.';

export const DUPLICATE_MARK_INFORMATION_TEXT = 'Potential duplicates have been found. If any of these matches are correct, please select the appropriate one and mark the current entity as a duplicate of the selected entity. If none of the matches are accurate, you can confirm that there are no exact duplicates and proceed with verification.';

export const ENTITY_MANDATORY_FIELDS = ['entityName', 'primaryAddressLine1', 'countryCode', 'city', 'state', 'postCode', 'entityOwnershipTypeCode'];
