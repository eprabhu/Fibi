export class DisclosureSFIs {
    isProjectPresent?: boolean;
    personEntities?: PersonEntity[];
    count?: number;
}

export interface PersonEntity {
    personEntityId?: number;
    personEntityNumber?: number;
    personId?: string;
    person?: null;
    entityId?: number;
    coiEntity?: CoiEntity;
    entityNumber?: number;
    isRelationshipActive?: boolean;
    versionNumber?: null;
    versionStatus?: string;
    sponsorsResearch?: null;
    involvementStartDate?: number;
    involvementEndDate?: number;
    studentInvolvement?: null;
    staffInvolvement?: null;
    instituteResourceInvolvement?: null;
    updateTimestamp?: null;
    updateUser?: null;
    createUser?: null;
    createTimestamp?: null;
    revisionReason?: null;
    personEntityRelationships?: null;
    validPersonEntityRelTypes?: ValidPersonEntityRelType[];
    validPersonEntityRelTypeCodes?: null;
    personFullName?: null;
    unit?: null;
    relationshipTypes?: null;
    designation?: null;
    updateUserFullName?: null;
    personEntityRelationshipDto?: PersonEntityRelationshipDto;
    sfiCompleted?: boolean;
    disclosureStatusCount?: any[];
}

export interface CoiEntity {
    entityId?: number;
    entityNumber?: number;
    entityName?: string;
    versionNumber?: null;
    versionStatus?: string;
    entityStatusCode?: null;
    entityStatus?: null;
    entityTypeCode?: string;
    entityType?: Type;
    riskCategoryCode?: null;
    entityRiskCategory?: null;
    phone?: null;
    countryCode?: null;
    country?: Country;
    city?: null;
    address?: null;
    zipCode?: null;
    emailAddress?: null;
    isActive?: boolean;
    webURL?: null;
    createUser?: null;
    createTimestamp?: null;
    updateUser?: null;
    updateTimestamp?: null;
    approvedUser?: null;
    approvedTimestamp?: null;
    revisionReason?: null;
    countryDescription?: null;
    entityTypeDescription?: null;
    riskLevelDescription?: null;
    statusDescription?: null;
    updatedUserFullName?: null;
    createUserFullName?: null;
    majorVersion?: boolean;
    newRiskCategory?: null;
}

export interface Country {
    countryCode?: null;
    countryName?: string;
    currencyCode?: null;
    currency?: null;
    updateTimeStamp?: null;
    updateUser?: null;
    countryTwoCode?: null;
}

export interface Type {
    entityTypeCode?: null;
    description?: string;
    updateTimestamp?: number | null;
    updateUser?: null | string;
    isActive?: boolean | null;
    disclosureTypeCode?: string;
    relationshipTypeCode?: string;
}

export interface PersonEntityRelationshipDto {
    personEntityId?: number;
    entityId?: number;
    entityName?: string;
    entityNumber?: number;
    countryName?: string;
    validPersonEntityRelType?: string;
    entityType?: string;
    entityRiskCategory?: string;
    personEntityVersionStatus?: string;
    isRelationshipActive?: boolean;
    involvementStartDate?: null;
    involvementEndDate?: null;
}

export interface ValidPersonEntityRelType {
    validPersonEntityRelTypeCode?: number;
    disclosureTypeCode?: string;
    coiDisclosureType?: Type;
    relationshipTypeCode?: string;
    personEntityRelType?: Type;
    description?: string;
    questionnaireNumber?: null;
    isActive?: boolean;
    updateTimestamp?: number;
    updateUser?: string;
}
