export type EntityRiskProxyController = '/organization' | '/sponsor' | '/compliance' | '';
export type EntityRiskCategoryCode = 'OR' | 'EN' | 'CO' | 'SP' | '';
export const ENTITY_DUPLICATE_MATCH_MODAL_ID = 'duplicate_entity_match_found';

export class Create_Entity {
    entityName: string = '';
    entityType: EntityType;
    primaryAddressLine1: string = '';
    primaryAddressLine2: string = '';
    city: string = '';
    state: string = '';
    postCode: number;
    countryCode: string = '';
    country: Country = new Country();
    phoneNumber: any;
    certifiedEmail: any = '';
    websiteAddress: any = '';
    dunsNumber: number;
    ueiNumber: number;
    cageNumber: number;
    humanSubAssurance?: any;
    anumalWelfareAssurance?: any;
    animalAccreditation?: any;
    isDunsMatched?: any;
    entityOwnershipTypeCode: any;
    businessTypeCode: any;
    entityOwnerShip: EntityOwnerShip = new EntityOwnerShip();
    entityStatusTypeCode: string;
}

export class EntityOwnerShip {
    description: string;
    isActive: boolean;
    ownershipTypeCode: any;
    updateTimestamp: any;
    updatedBy: any;
}

interface EntityType {
    description: string;
    entityTypeCode: number;
    isActive: boolean;
    updateTimestamp: number;
    updateUser: string;
}

export class IndustryDetails {
    entityId: any;
    entityIndustryCatIds: any = [];
    primaryCatId: any;
    updatePrimaryCatId = false;
}

export class RegistrationDetails {
    entityId?: number;
    entityRegistrationId?: number;
    entityMailingAddressId?: number;
    regTypeCode: any = '';
    regNumber: any = '';
}

export class AdditionalAddress {
    entityId?: number;
    entityMailingAddressId?: number;
    addressLine1: string = '';
    addressLine2: string = '';
    city: string = '';
    state: string = '';
    postCode: number;
    countryCode: string = '';
    addressType?: any;
    addressTypeCode: string = '';
}

export class OtherDetails {
    startDate?: string = '';
    incorporationDate?: string = '';
    incorporatedIn?: string = '';
    congressionalDistrict?: string = '';
    federalEmployerId?: string = '';
    priorName?: string;
    foreignName?: string;
    shortName?: string;
    numberOfEmployees?: number = 0;
    businessTypeCode?: any;
    activityText?: string = '';
    currencyCode?: string = '';
    entityBusinessType: any;
}

export class EntityRisk {
    riskTypeCode = '';
    riskLevelCode = '';
    description = '';
    entityId: number | null = null;
    entityRiskId?: number | null = null;
    entity?: EntityDetails;
    riskLevel?: RiskLevel;
    updateTimestamp?: number;
    updatedBy?: string;
    riskType?: RiskType;
}
export class EntityRiskRO {
    entityId?: any;
    description?: string = '';
    riskTypeCode?: string = '';
    riskLevelCode?: string = '';
    entityRiskId?: number | null = null;
    riskType?: string = '';
    riskLevel?: string = '';
    oldRiskLevel?: string = '';
    oldRiskLevelCode?: string = '';
    oldDescription?: string = '';
}

export class RiskType {
    riskTypeCode: string;
    riskCategoryCode: string;
    description = '';
    updateTimestamp: number;
    updatedBy: string;
    isActive: boolean;
}

export interface EntityStatusType {
    entityStatusTypeCode?: string;
    description?: string;
    updateTimestamp?: number;
    updatedBy?: string;
    isActive?: boolean;
}

export class RiskLevel {
    riskLevelCode: string;
    description = '';
    updateTimestamp: number;
    updatedBy: string;
    isActive: boolean;
}

export class OtherReferenceId {
    externalIdTypeCode: any;
    externalId: any = '';
    description: string = '';
    entityId?: any;
    entityExternalMappingId?: number;
}

export class EntireEntityDetails {
    priorNames?: any[] = [];
    foreignNames?: any[] = [];
    entityRisks?: EntityRisk[] = [];
    entityRegistrations?: EntityRegistration[] = [];
    entityMailingAddresses?: any[] = [];
    entityDetails? = new EntityDetails();
    attachments?: EntityAttachment[] = [];
    entityTabStatus? = new EntityTabStatus();
    entityIndustryClassifications?: any[] = [];
    entityExternalIdMappings?: EntityExternalIdMappings[] = [];
    originalName: string;
}

export class EntityRegistration {
    entityRegistrationId?: number;
    entityId?: number;
    entity?: EntityDetails;
    regTypeCode?: string;
    registrationType?: RegistrationType;
    regNumber?: string;
    isActive?: boolean | null;
    updateTimestamp?: number;
    updatedBy?: string;
}

export class RegistrationType {
    regTypeCode?: string;
    description?: string;
    updateTimestamp?: number;
    updatedBy?: string;
    isActive?: boolean;
}

export class EntityTabStatus {
    entity_overview? = false;
    entity_sub_org_info? = false;
    entity_sponsor_info? = false;
    organization_feed_status: string;
    organization_id: string;
    sponsor_feed_status: string;
    sponsor_code: string;
    organization_feed_status_code: any;
    sponsor_feed_status_code: any;
}

export class EntityAttachmentDetails {
    entityAttachmentId: number;
    attachmentNumber: number;
    versionNumber: number;
    versionStatus: any;
    entityId: number;
    comment: string;
    attachmentTypeCode: string;
    attachmentType: EntityAttachmentType;
    attachmentStatusCode: any;
    attachmentStatus: any;
    fileName: string;
    mimeType: string;
    fileDataId: string;
    updateTimestamp: number;
    updatedBy: string;
    updateUserFullame: string;
}

export interface EntityAttachmentType {
    attachmentTypeCode?: string
    description?: string
    updateTimestamp?: number
    updatedBy?: string
    isActive?: boolean
    isPrivate?: boolean
}

export class EntityDetails {
    entityId?: number;
    entityName?: string;
    foreignName?: any;
    priorName?: any;
    shortName?: string;
    dunsNumber?: string;
    ueiNumber?: any;
    cageNumber?: any;
    websiteAddress?: string;
    startDate?: any;
    incorporationDate?: any;
    certifiedEmail?: string;
    activityText?: string;
    phoneNumber?: string;
    primaryAddressLine1?: string;
    primaryAddressLine2?: string;
    city?: string;
    state?: string;
    postCode?: string;
    humanSubAssurance?: any;
    anumalWelfareAssurance?: any;
    animalAccreditation?: any;
    approvedBy?: string;
    approvedTimestamp?: number;
    createdBy?: string;
    createTimestamp?: number;
    updatedBy?: string;
    updateTimestamp?: number;
    entityStatusTypeCode?: string;
    operatingStatusTypeCode?: any;
    businessTypeCode?: string;
    currencyCode?: string;
    entitySourceTypeCode?: any;
    countryCode?: string;
    entityOwnershipTypeCode?: string;
    incorporatedIn?: string;
    congressionalDistrict?: string;
    federalEmployerId?: string;
    numberOfEmployees?: number;
    entityNumber?: number;
    versionNumber?: number;
    versionStatus?: string;
    isActive?: boolean;
    isDunsMatched?: boolean;
    entityStatusType?: EntityStatusType;
    entityOperatingStatusType?: any;
    entitySourceType?: any;
    originalEntityId: any;
    documentStatusTypeCode?: any;
    country?: Country;
    entityOwnershipType?: EntityOwnershipType;
    entityBusinessType?: EntityBusinessType;
    entityDocumentStatusType?: EntityDocumentStatusType;
}

export interface EntityDocumentStatusType {
    documentStatusTypeCode?: string
    description?: string
    updateTimestamp?: number
    updatedBy?: string
    isActive?: boolean
}

export interface EntityOwnershipType {
    ownershipTypeCode?: string
    description?: string
    updateTimestamp?: number
    updatedBy?: string
    isActive?: boolean
}

export interface EntityBusinessType {
    businessTypeCode?: string
    description?: string
    updateTimestamp?: number
    updatedBy?: string
    isActive?: boolean
}

export class Country {
    countryCode?: string;
    countryName: string;
    currencyCode?: string;
    currency?: Currency
    updateTimeStamp?: any;
    updateUser?: string;
    countryTwoCode?: any;
}

export class Currency {
    currencyCode: string;
    currency: string;
    currencySymbol: any;
    updateUser: string;
    updateTimeStamp: any;
}

export function showEntityToast(type: 'SUCCESS' | 'ERROR') {
    let successToast = document.getElementById('success-toast');
    let errorMsg = document.getElementById('error-toast');
    if (type === 'SUCCESS') {
        if (successToast) {
            successToast.classList.remove('invisible');
        }
        if (errorMsg) {
            errorMsg.classList.add('invisible');
        }
    } else {
        if (errorMsg) {
            errorMsg.classList.remove('invisible');
        }
        if (successToast) {
            successToast.classList.add('invisible');
        }
    }
}

export function removeToast(type: 'SUCCESS' | 'ERROR') {
    let successToast = document.getElementById('success-toast');
    let errorMsg = document.getElementById('error-toast');
    if (type === 'SUCCESS') {
        if (successToast) {
            successToast.classList.add('invisible');
        }
    } else {
        if (errorMsg) {
            errorMsg.classList.add('invisible');
        }
    }
}

export class EntitySponsorRisk {
    riskTypeCode?: any = '';
    riskLevelCode?: any = '';
    description?: string = '';
    entityId?: any;
    entityRiskId?: any;
}

export class EntityExternalIdMappings {
    entityExternalMappingId?: number;
    entityId?: number;
    entity?: any;
    externalIdTypeCode?: string;
    entityExternalIdType?: EntityExternalIdType;
    entityExternalIdTypeDescription?: any;
    externalId?: string;
    description?: string;
    sponsorCode?: any;
    organizationId?: any;
    updatedBy?: string;
    updateTimestamp?: number;
}

export interface EntityExternalIdType {
    externalIdTypeCode: string;
    description: string;
    updateTimestamp: number;
    updatedBy: string;
    isActive: boolean;
}

export class SubAwardOrganization {
    attachments?: any[] = [];
    entityRisks?: EntityRisk[] = [];
    subAwdOrgDetailsResponseDTO? = new SubAwardOrganizationDetails();
}

export class SubAwardOrganizationDetails {
    id?: number | null = null;
    entityId?: string | number | null = null;
    organizationId?: number | null = null;
    entityOrganizationType: EntityOrganizationType = new EntityOrganizationType();
    samExpirationDate?: any | null = null;
    subAwdRiskAssmtDate?: any | null = null;
}
export class EntitySponsor {
    attachments?: any[] = [];
    entityRisks?: EntityRisk[] = [];
    sponsorDetailsResponseDTO? = new SponsorDetails();
}

export class SponsorDetails {
    id?: number | null = null;
    acronym?: string | null = null;
    entityId?: number | null = null;
    sponsorCode?: string | null = null;
    sponsorType?: SponsorType = new SponsorType();
}
export class SponsorType {
    code: string;
    description: string;
    budgetCategoryCode: any;
    fromGlMapping: any;
    toGlMapping: any;
    isActive: boolean;
}

export class EntityOrganizationType {
    organizationTypeCode: string;
    description: string;
    updateTimestamp: number;
    updatedBy: string;
    isActive: boolean;
}

export class EntityRiskModalDetails {
    entityRisk = new EntityRisk();
    selectedRiskTypeLookUpList: EntityRisk[] = [];
    selectedRiskLevelLookUpList: RiskLevel[] = [];
}

export class SaveAttachmentRo {
    sectionCode: string;
    newAttachments = new NewAttachments();
}
export class NewAttachments {
    fileName?: string;
    mimeType?: string;
    attachmentTypeCode?: string;
    entityId?: number;
    comment?: string;
    fileDataId?: null;
    attachmentnumber?: number;
    versionNumber?: number;
}

export class OverallAttachmentList {
    General: EntityAttachment[];
    Sponsor: EntityAttachment[];
    Organization: EntityAttachment[];
    Compliance: EntityAttachment[];
}

export interface EntityAttachment {
    entityAttachmentId?: number;
    attachmentNumber?: number;
    versionNumber?: number;
    versionStatus?: string
    entityId?: string | number;
    entity?: EntityDetails;
    comment?: string;
    attachmentTypeCode?: string;
    attachmentType?: EntityAttachmentType;
    attachmentStatusCode?: any;
    attachmentStatus?: any;
    fileName?: string;
    mimeType?: string;
    fileDataId?: string;
    updateTimestamp?: number;
    updatedBy?: string;
    updateUserFullame?: string;
    versionList?: EntityAttachment[];
}

export class EntitySectionDetails {
    sectionId = '';
    sectionName = '';
    subSectionId: number | null = null;
}

export class EntityCardDetails {
    entityName?: string;
    primaryAddress?: string;
    city?: string;
    state?: string;
    country?: Country;
    dunsNumber?: any;
    ueiNumber?: any;
    cageNumber?: any;
    website?: string;
    email?: string;
    phone?: any;
    phoneNumber?: any;
    sponsorCode?: any;
    organizationId?: any;
    matchQualityInformation?: any;
    postalCode?: any;
    entityId?: any;
    primaryAddressLine1?: string;
    primaryAddressLine2?: string;
    duplicateEntityDetails?: EntityCardDetails;
}

export class DuplicateCheckObj{
    entityName : string;
    primaryAddressLine1: string;
    primaryAddressLine2: string;
    countryCode: string;
}

export class EntityDupCheckConfig{
    duplicateView: 'MODAL_VIEW' | 'CARD_VIEW' = 'MODAL_VIEW';
    modalHeader?: string = 'Matching Entities Found'; //based on mode optional
    modalPrimaryButton?: string = 'Create New';
    modalHelpText?: string = `The details you entered match the following entities in our system. Please review the list below. If you still wish to create a new entity, you can skip this step and click on '${this.modalPrimaryButton}'.`;
}

export class DupMarkingModalConfig {
    modalHeader: string =  'Confirmation';
    modalPrimaryButton: string = 'Mark this as duplicate';
    modalHelpText: string = 'The currently opened entity should be marked as a duplicate of the following entity.';
}

export class EntityDetailsInPopup {
    entityName: string;
    entityId: any;
    fullAddress: string;
    phone: any;
    website: string;
    email: string;
}

export class DuplicateMarkingAPIReq {
    originalEntityId: any;
    duplicateEntityId: any;
    description: string;
}

export class SubAwardOrgUpdateClass {
    entityId: number;
    subAwardOrgFields = new SubawardOrgFields();
}

export class SubawardOrgFields {
    samExpirationDate?: any;
    organizationTypeCode?: any;
    subAwdRiskAssmtDate?: any;
    feedStatusCode?: any;
}

export class SponsorUpdateClass {
    entityId: number;
    entitySponsorFields = new SponsorFields();
}

export class SponsorFields {
    sponsorCode?: any;
    sponsorTypeCode?: any;
    acronym?: string;
    feedStatusCode?: any;
}

//changes during modification in the following fields need to update feed status of sponsor
export const SPONSOR_FIELDS = [
    'entityName', 'sponsorTypeCode', 'countryCode', 'state', 'postCode', 'ueiNumber', 'dunsNumber', 'cageNumber', 'acronym'
]

//changes during modification in the following fields need to update feed status of organization
export const ORGANISATION_FIELDS = [
    'entityName', 'numberOfEmployees', 'federalEmployerId', 'ueiNumber', 'cageNumber', 'humanSubAssurance', 'anumalWelfareAssurance', 'samExpirationDate',
    'incorporatedIn', 'incorporationDate', 'congressionalDistrict', 'phoneNumber', 'primaryAddressLine1', 'primaryAddressLine2', 'city', 'state', 'postCode', 'countryCode',
    'dunsNumber', 'organizationTypeCode'
]
