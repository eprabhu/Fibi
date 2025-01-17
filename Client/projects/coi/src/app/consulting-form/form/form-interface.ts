export interface EntityStatus {
    entityStatusCode: string;
    description: string;
    updateTimestamp: number;
    updateUser: string;
    isActive: boolean;
  }

  export interface EntityType {
    entityTypeCode: string;
    description: string;
    updateTimestamp: number;
    updateUser: string;
    isActive: boolean;
  }

  export interface Currency {
    currencyCode: string;
    currency: string;
    currencySymbol?: string;
    updateUser: string;
    updateTimestamp: number;
  }

  export class Country {
    countryCode: string;
    countryName: string;
    currencyCode: string;
    currency: Currency;
    updateTimestamp: number;
    updateUser: string;
    countryTwoCode: string;
  }

  export interface EntityRiskCategoryCode {
    description: string;
    riskCategoryCode: string;
    isActive?: boolean;
    updateUser?: string;
    updateTimestamp?:number;
  }

  export class CoiEntity {
    entityId: number;
    entityName: string;
    entityStatusCode: string;
    entityStatus: EntityStatus;
    riskCategoryCode?: string = null;
    entityRiskCategory?: EntityRiskCategoryCode;
    entityTypeCode?: string = null;
    entityType?: EntityType;
    countryCode?: string;
    country?: Country = new Country();
    phone?: string;
    city?: string;
    address?: string;
    zipCode: string;
    versionNumber: string;
    versionStatus: string;
    emailAddress: string;
    webURL?: string;
    createTimestamp: number;
    createUser: string;
    updateTimestamp: number;
    updateUser: string;
    approvedTimestamp?: number;
    approvedUser?: string;
    isActive: boolean;
    entityNumber?: number;
    revisionReason?: string;
    majorVersion?: boolean;

  }

  export class EntityDetails {
    coiDisclosureOld: any;
    person: any;
    numberOfSFI: any;
    coiFinancialEntity: any;
    personEntity: any;
    comment: any;
    coiEntity: CoiEntity = new CoiEntity();
  }

  export class EntitySaveRO {
    entityId: number;
    entityNumber: number;
    sponsorsResearch: boolean;
    involvementStartDate: string;
    staffInvolvement: string;
    studentInvolvement: string;
    instituteResourceInvolvement: string;
    validPersonEntityRelTypeCodes: number[];
 }

 export class ExistingEntityDetails {
    personEntityId: number;
    entityNumber: number;
    personEntityRelationships: any = {};
    isEntityAvailable: boolean;
 }
