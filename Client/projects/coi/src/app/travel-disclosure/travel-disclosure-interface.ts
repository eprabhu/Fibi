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
