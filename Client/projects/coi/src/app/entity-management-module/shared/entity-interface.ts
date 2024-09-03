export type EntityRiskProxyController = '/organization' | '/sponsor' | '/compliance' | '';
export type EntityRiskCategoryCode = 'OR' | 'EN' | 'CO' | 'SP' | '';
export type AttachmentInputType = 'REPLACE' | 'ADD' | 'DESCRIPTION_CHANGE' | '';

export class Create_Entity {
    entityName: string = '';
    entityType: EntityType;
    primaryAddressLine1: string = '';
    primaryAddressLine2: string = '';
    city: string = '';
    state: string = '';
    postCode: number;
    countryCode: string = '';
    country: Country;
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
    riskTypeCode: any = '';
    riskLevelCode: any = '';
    description: string = '';
    entityId: any;
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
}

export interface RiskType {
    riskTypeCode: string;
    riskCategoryCode: string;
    description: string;
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

export interface RiskLevel {
    riskLevelCode: string;
    description: string;
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

export class EntityDetailsCard {
    entityName: string = 'Google';
    entityMatchPercent?: number = 10;
    entityAddress: string = '122; Parkway in Mountain View; California';
    DUNSNumber: any = '1221212121';
    country: any = 'USA';
    city: any = 'Mountain View';
    website: any = 'www.google.com';
    email: any = 'google@gmail.com';
    industry: any = 'Cloud Computing';
    state: any = 'Texas';
    UEINumber: any = '1212121212';
    CAGENumber: any = '32121212212';
}

export class EntireEntityDetails {
    priorNames?: any[] = [];
    foreignNames?: any[] = [];
    entityRisks?: EntityRisk[] = [];
    entityRegistrations?: any[] = [];
    entityMailingAddresses?: any[] = [];
    entityDetails? = new EntityDetails();
    attachments?: EntityAttachment[] = [];
    entityTabStatus? = new EntityTabStatus();
    entityIndustryClassifications?: any[] = [];
    entityExternalIdMappings?: EntityExternalIdMappings[] = [];
}

export class EntityTabStatus {
    entity_overview? = false;
    entity_sub_org_info? = false;
    entity_sponsor_info? = false;
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
    country?: Country;
    entityOwnershipType?: EntityOwnershipType;
    entityBusinessType?: EntityBusinessType;
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
    sponsorTypeCode?: string | null = null;
}
export class SubAwardOrganizationDetails {
    id?: number | null = null;
    entityId?: string | number | null = null;
    organizationId?: number | null = null;
    organizationTypeCode?: string | null = null;
    samExpirationDate?: any | null = null;
    subAwdRiskAssmtDate?: any | null = null;
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

export interface AttachmentSaveRO {
    fileName: string;
    mimeType: string;
    attachmentTypeCode: string | number;
    comment: string;
    fileDataId: string | null;
}

export interface AttachmentReplaceRO {
    fileName: string;
    mimeType: string;
    attachmentTypeCode: string | number;
    comment: string;
    fileDataId: string | null;
    attachmentNumber: number;
    versionNumber: number;
}
