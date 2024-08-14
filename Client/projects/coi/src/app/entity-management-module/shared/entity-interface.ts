export class Create_Entity{
    primaryName: string = '';
    entityType: EntityType;
    primaryAddressLine1: string = '';
    primaryAddressLine2: string = '';
    city: string = '';
    state: string = '';
    postCode: number;
    countryCode: string = '';
    phoneNumber: any;
    certifiedEmail: any = '';
    websiteAddress: any = '';
    dunsNumber: number;
    ueiNumber: number;
    cageNumber: number;
    humanSubAssurance?: any;
    animalWelfare?: any;
    animalAccrediation? : any;
}

interface EntityType {
    description: string;
    entityTypeCode: number;
    isActive: boolean;
    updateTimestamp: number;
    updateUser: string;
}

export class Industry_Details{
    industryCategoryType: any = '';
    industryCategroyDescription: any = [];
}

export class RegistrationDetails {
    entityId?: number;
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
    addressType: string = '';
}

export class OtherDetails {
    startDate?: string = '';
    incorporationDate?: string = '';
    incorporationIn?: string = '';
    congressionalDistrict?: string = '';
    federalEmployerId?: string = '';
    entityPriorName?: Array<string> = [];
    entityAlternateName?: Array<string> = [];
    entityShortName?: string;
    numberofemployees?: number = 0;
    entityBusinessStatus?: any;
    activityText?: string = '';
    currencyCode?: string = '';
}

export class EntityRisk {
    riskType: any = '';
    riskLevel: any = '';
    riskDescription: string = '';
}

export class OtheReferenceId {
    referenceType: any;
    referenceId: any = '';
    description: string = '';
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
        primaryName: string;
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
