export class Create_Entity{
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
    animalAccreditation? : any;
    entityOwnershipTypeCode: any;
    entityOwnerShip: EntityOwnerShip;
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

export class IndustryDetails{
   entityId: any;
   entityIndustryCatIds: any = [];
   primaryCatId: any;
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
}

export class EntityRisk {
    riskTypeCode: any = '';
    riskLevelCode: any = '';
    description: string = '';
    entityRiskId?: any;
    entityId: any;
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
    entityDetails: EntityDetails;
    entityIndustryClassifications:any = [];
    entityMailingAddresses:any = [];
    entityRegistratios:any = [];
}

export class EntityDetails {
        entityId: string|number;
        entityName: string;
        foreignName: string;
        priorName: string;
        shortName: string;
        dunsNumber: any;
        ueiNumber: any;
        cageNumber: any;
        websiteAddress: string;
        startDate: any;
        incorporationDate: any;
        certifiedEmail: string;
        activityText: any;
        primaryAddressLine1: string;
        primaryAddressLine2: string;
        city: string;
        state: string;
        postCode: any;
        isPubliclyTradedCompany: string;
        approvedBy: string;
        approvedTimestamp: string;
        createdBy: string;
        createTimestamp: string;
        updatedBy: any;
        updateTimestamp: any;
        entityStatusTypeCode: string;
        operatingStatusTypeCode: any;
        businessEntityType: string;
        currencyCode: string;
        entitySourceTypeCode: string;
        countryCode: string;
        entityStatusType: string;
        entityOperatingStatusType: string;
        entitySourceType: string;
        country: Country;
        phoneNumber: any;
        congressionalDistrict: any;
        incorporatedIn: any;
        numberOfEmployees: any;
        federalEmployerId: any;
}

export class Country {
        countryCode: string;
        countryName: string;
        currencyCode: string;
        currency: Currency
        updateTimeStamp: any;
        updateUser: string;
        countryTwoCode: any;
}

export class Currency {
    currencyCode: string;
    currency: string;
    currencySymbol: any;
    updateUser: string;
    updateTimeStamp: any;
}

export function showEntityToast(type: 'SUCCESS'|'ERROR') {
    let successToast = document.getElementById('success-toast');
    let errorMsg = document.getElementById('error-toast');
    if(type === 'SUCCESS') {
        if(successToast) {
            successToast.classList.remove('invisible');
        }
        if(errorMsg) {
            errorMsg.classList.add('invisible');
        }
    } else {
        if(errorMsg) {
            errorMsg.classList.remove('invisible');
        }
        if(successToast) {
            successToast.classList.add('invisible');
        }
    }
}

export class EntitySponsorRisk {
    riskTypeCode: any = '';
    riskLevelCode: any = '';
    description: string = '';
    entityId: any;
    entityRiskId: any;
}

export class SponsorDetails {
    id?: number;
    entityId?: any;
    acronym?: string = '';
    sponsorTypeCode?: string = '';
}

export class EntityExternalIdMappings {
    entityExternalMappingId: number;
    entityId: number;
    entity: any;
    externalIdTypeCode: string;
    entityExternalIdType: EntityExternalIdType;
    entityExternalIdTypeDescription: any;
    externalId: string;
    description: string;
    sponsorCode: any;
    organizationId: any;
    updatedBy: string;
    updateTimestamp: number;
}



export interface EntityExternalIdType {
    externalIdTypeCode: string;
    description: string;
    updateTimestamp: number;
    updatedBy: string;
    isActive: boolean;
}
