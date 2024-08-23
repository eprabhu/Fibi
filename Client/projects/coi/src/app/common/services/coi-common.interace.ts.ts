export type Method = 'SOME' | 'EVERY';
export type GlobalEventNotifierUniqueId = 'CREATE_NEW_TRAVEL_DISCLOSURE' | 'COI_OPA_HEADER' | 'COI_DISCLOSURE_HEADER' | 'SCROLL_SPY' | '';
export type GlobalEventNotifier = { uniqueId: GlobalEventNotifierUniqueId, content?: any };
export type LoginPersonDetailsKey = keyof LoginPersonDetails;

export class LoginPersonDetails {
    personID: any;
    userName: any;
    firstName: any;
    lastName: any;
    fullName: any;
    unitNumber: any;
    unitName: any;
    primaryTitle: any;
    email: any;
    isUnitAdmin: any;
    login: any;
    userType: any;
    secretImageUri: any;
    isExternalUser: any;
    gender: any;
}
