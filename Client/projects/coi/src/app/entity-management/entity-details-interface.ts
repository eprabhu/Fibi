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
  currencySymbol: string | null;
  updateUser: string;
  updateTimestamp: number;
}

export interface Country {
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
  country?: Country;
  phone: string | null;
  city: string | null;
  address: string | null;
  zipCode: string;
  versionNumber: string;
  versionStatus: string;
  emailAddress: string;
  webURL: string;
  createTimestamp: number;
  createUser: string;
  updateTimestamp: number;
  updateUser: string;
  approvedTimestamp: number | null;
  approvedUser: string | null;
  isActive: boolean;
}

export class EntityDetails {
  coiDisclosureOld: any;
  person: any;
  numberOfSFI: any;
  coiFinancialEntity: any;
  personEntity: any;
  coiEntity: CoiEntity = new CoiEntity();
}

