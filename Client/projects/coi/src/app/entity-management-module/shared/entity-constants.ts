const BASIC_DETAILS =  {sectionName: 'Basic Details', sectionId: 'EO101'};
const COMPANY_DETAILS = {sectionName: 'Company Details', sectionId: 'EO102'};
const ENTITY_RISK = {sectionName: 'Entity Risk', sectionId: 'EO103'};
const OTHER_REFERENCE_IDS = {sectionName: 'Other Reference IDs', sectionId: 'EO104'};
const ADDITIONAL_INFORMATION =  {sectionName: 'Additional Information', sectionId: 'EO105'};

const SPONSOR_DETAILS =  {sectionName: 'Sponsor Details', sectionId: 'ES201'};
const SPONSOR_RISK =  {sectionName: 'Sponsor Risk', sectionId: 'ES202'};
const SPONSOR_ATTACHMENTS = {sectionName: 'Sponsor Attachments', sectionId: 'ES203'};
const SPONSOR_QUESTIONNAIRE = {sectionName: 'Sponsor Questionnaire', sectionId: 'ES204'};

const SUB_AWARD_ORGANISATION = {sectionName: 'Subaward Organization Details', sectionId: 'ES301'};
const SUB_AWARD_RISK = {sectionName: 'Subaward Organization Risk', sectionId: 'ES302'};
const SUB_AWARD_ATTACHMENTS = {sectionName: 'Subaward Organization Attachments', sectionId: 'ES303'};
const SUB_AWARD_QUESTIONNAIRE = {sectionName: 'Subaward Organization Questionnaire', sectionId: 'ES304'};

const COMPLIANCE_RISK = {sectionName: 'Compliance Risk', sectionId: 'EC401'};
const COMPLIANCE_ATTACHMENTS = {sectionName: 'Compliance Attachments', sectionId: 'EC402'};
const COMPLIANCE_QUESTIONNAIRE = {sectionName: 'Compliance Questionnaire', sectionId: 'EC403'};

const ATT_ENTITY_ATTACHMENTS = {sectionName: 'Entity Attachments', sectionId: 'EA501'};
const ATT_SPONSOR_ATTACHMENTS = {sectionName: 'Sponsor Attachments', sectionId: 'EA502'};
const ATT_ORGANISATION_ATTACHMENTS = {sectionName: 'Organization Attachments', sectionId: 'EA503'};
const ATT_COMPLIANCE_ATTACHMENTS = {sectionName: 'Compliance Attachments', sectionId: 'EA504'};

export const OverviewTabSection = new Map([
    ['BASIC_DETAILS' , BASIC_DETAILS],
    ['COMPANY_DETAILS' , COMPANY_DETAILS],
    ['ENTITY_RISK' , ENTITY_RISK],
    ['OTHER_REFERENCE_IDS' , OTHER_REFERENCE_IDS],
    ['ADDITIONAL_INFORMATION' , ADDITIONAL_INFORMATION],
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
    ['Additional_Information' , ADDITIONAL_INFORMATION],
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

