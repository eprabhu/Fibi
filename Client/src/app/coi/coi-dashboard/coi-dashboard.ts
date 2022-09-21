export class CoiDashboardRequestObject {
    advancedSearch = 'L';
    pageNumber = 30;
    sort = {'createTimestamp': 'asc'};
    tabName = '';
    isDownload = false;
}

export class SFIDashboardRequestObject {
    advancedSearch = 'L';
    pageNumber = 30;
    sort = {};
    isDownload = false;
    property16 = '';
    property8 = '';
    property17 = null;
    property18 = null;
    property19 = null;
}

export interface Disclosure {
    coiDisclosureId: number;
    coiDisclosureNumber: string;
    createTimestamp: any;
    discActiveStatus: string;
    disclosureDisposition: string;
    disclosureDispositionCode: number;
    disclosureStatus: string;
    disclosureStatusCode: number;
    dispositionStatus: string;
    disclosureSequenceStatusCode: number;
    documentNumber: number;
    expirationDate: number;
    fullName: string;
    lastApprovedVersion: number;
    moduleItemKey: number;
    numberOfAwards: number;
    numberOfProposals: number;
    numberOfSfi: number;
    personId: number;
    reviewStatus: string;
    sequenceNumber: number;
    submittedDate: number;
    updateTimeStamp: number;
    updateUser: string;
    noOfAwardInActive: any;
    noOfProposalInActive: any;
    noOfProposalInPending: any;
    noOfAwardInPending: any;
}
