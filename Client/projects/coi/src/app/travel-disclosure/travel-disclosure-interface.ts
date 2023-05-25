export class CoiTravelDisclosure {
    travelDisclosureId: number;
    travelNumber: number;
    versionNumber: number;
    versionStatus = 'ACTIVE';
    personId: string
    entityId: number;
    entityNumber: number;
    travelStatusCode: string;
    isSponsoredTravel: boolean;
    travelTitle: string;
    purposeOfTheTrip: string;
    travelAmount: number;
    travelStartDate: any;
    noOfDays: number;
    destinationCity: string;
    destinationCountry: string;
    relationshipToYourResearch: string;
    travelState: string;
    travelEndDate: any;
    acknowledgeBy: string;
    acknowledgeAt: string;
    travelTravellerId: string;
    travellerTypeCode: Array<string> = [];
    isInternationalTravel: boolean;
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