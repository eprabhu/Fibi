export class CoiTravelDisclosure {
    travellerTypeCode: Array<string> = [];
    entityId: number;
    entityNumber: number;
    travelTitle: string;
    travelState: string;
    destinationCity: string;
    destinationCountry: string;
    purposeOfTheTrip: string;
    relationshipToYourResearch: string;
    travelAmount: number;
    travelStartDate: any;
    travelEndDate: any;
    isInternationalTravel: boolean;
    isSponsoredTravel: boolean;
    personId: string;
    noOfDays: number;
    homeUnit: string;
    description: string;
    travelDisclosureId?: number;
    travelEntityName?: string;
}

export interface TravelDisclosureTraveller {
    description: string;
    isActive: boolean;
    isChecked?: boolean;
    travelerTypeCode?: string;
    travelStatusCode?: string;
    updateTimestamp: number;
    updateUser: string;
}

export interface TravelCreateModalDetails {
    homeUnit: null;
    description: string;
    personId: string;
    homeUnitName: null;
}

export class TravelDisclosureResponseObject {
    travelDisclosureId: number;
    versionStatus: string;
    entityId: number;
    entityNumber: number;
    travelEntityName: string;
    travelTitle: string;
    purposeOfTheTrip: string;
    travelAmount: number;
    travelStartDate: number;
    travelEndDate: number;
    destinationCity: string;
    destinationCountry: string;
    travelState: string;
    relationshipToYourResearch: string;
    acknowledgeBy: string;
    acknowledgeAt: number;
    updateTimestamp: number;
    updateUser: string;
    createUser: string;
    createTimestamp: number;
    travellerHomeUnit: string;
    description: string;
    travelSubmissionDate: number;
    travelDisclosureStatus: string;
    travelDisclosureStatusCode: string;
    dispositionStatus: string;
    dispositionStatusCode: string;
    reviewStatus: string;
    reviewStatusCode: string;
    adminPersonId: string;
    adminGroupId: number;
    adminPersonName: string;
    adminGroupName: string;
    homeUnitNumber: string;
    homeUnitName: string;
    isInterNationalTravel: boolean;
    travellerTypeCodeList: {};
    personId: string;
    personFullName: string;
    entityTypeCode: string;
    entityType: string;
    countryCode: string;
    country: string;
    certifiedBy: string;
    certifiedAt: number;
    documentStatusCode: string;
    documentStatus: string;
    disclosureStatus: string;
    disclosureStatusCode: string;
}

export interface EndpointOptions {
    contextField: string;
    formatString: string;
    path: string;
    defaultValue: string;
    params: string;
}

export class EntityData {
    country: string;
    entityId: string | number;
    entityType: string;
    entityName: string;
}

export interface TravelHistoryRO {
    personId: String;
    entityNumber: number;
}

export interface TravelHistory {
    travelDisclosureId: number;
    travelEntityName: string;
    entityType: string;
    country: string;
    travelTitle: string;
    purposeOfTheTrip: string;
    destinationCity: string;
    destinationCountry: string;
    destinationState: string;
    travellerTypeCodeList: [];
    travelAmount: number;
    travelStartDate: number;
    travelEndDate: number;
}

export class DefaultAdminDetails {
    adminPersonId = '';
    adminGroupId = null;
    adminPersonName = '';
    adminGroupName = '';
}
