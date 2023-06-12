export class CoiTravelDisclosure {
    versionNumber: string;
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
    travelDisclosureId: any;
    versionStatus: string;
    entityId: number;
    entityNumber: any;
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
    acknowledgeBy: any;
    acknowledgeAt: any;
    updateTimestamp: number;
    updateUser: string;
    createUser: string;
    createTimestamp: number;
    travellerHomeUnit: string;
    description: any;
    travelSubmissionDate: any;
    travelDisclosureStatus: string;
    travelDisclosureStatusCode: string;
    dispositionStatus: any;
    dispositionStatusCode: string;
    reviewStatus: string;
    reviewStatusCode: string;
    adminPersonId: string;
    adminGroupId: number;
    adminPersonName: any;
    adminGroupName: any;
    homeUnitNumber: string;
    homeUnitName: string;
    isInterNationalTravel: boolean;
    travellerTypeCodeList: string[];
    personId: string;
    personFullName: string;
  }

export interface EndpointOptions {
    contextField: string;
    formatString: string;
    path: string;
    defaultValue: string;
    params: string;
}
