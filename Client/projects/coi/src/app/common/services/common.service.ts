import {Injectable} from '@angular/core';
import {HttpClient, HttpHeaders} from '@angular/common/http';
import {BehaviorSubject, Subject} from 'rxjs';
import {environment} from '../../../environments/environment';
import {getFromLocalStorage, setIntoLocalStorage} from '../../../../../fibi/src/app/common/utilities/user-service';
import {ElasticConfigService} from '../../../../../fibi/src/app/common/services/elastic-config.service';
import {Toast} from 'bootstrap';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { getPersonLeadUnitDetails } from '../utilities/custom-utilities';
import { Router } from '@angular/router';

type Method = 'SOME' | 'EVERY';
@Injectable()
export class CommonService {

    isShowLoader = new BehaviorSubject<boolean>(true);
    isManualLoaderOn = false;
    isShowOverlay = false;
    baseUrl = '';
    fibiUrl = '';
    authUrl = '';
    opaUrl = '';
    formUrl = '';
    currencyFormat = '$';
    forbiddenModule = '';
    isEvaluation: boolean;
    isMapRouting: boolean;
    isEvaluationAndMapRouting: boolean;
    cvFileType: any = [];
    claimFileType: any = [];
    enableSSO = false;
    rightsArray: any = [];
    isIE = /msie\s|trident\//i.test(window.navigator.userAgent);
    isValidUser = false;
    extension: any = [];
    currentUserDetails: any = {};
    isWafEnabled: boolean;
    canAddOrganization: boolean;
    isGrantCallStatusAutomated = false;
    isManpowerEnabled = false;
    isEnableAddtoAddressBook = false;
    isDevProposalVersioningEnabled = false;
    isExternalUser = false;
    isCreateAgreement = false;
    isShowAgreementSupport = false;
    isShowAgreementNotifyAction = false;
    isElasticAuthentiaction = false;
    isCoiReviewer = false;
    isOPAReviewer = false;
    elasticUserName = '';
    elasticAuthScheme = '';
    elasticDelimiter = '';
    elasticPassword = '';
    elasticIndexUrl = '';
    generalFileType = 'pdf';
    appLoaderContent = '';
    isEnableLock = false;
    isPreventDefaultLoader = false;
    timer: any;
    appToastContent = '';
    toastClass = 'success';
    dashboardModules: any = {};
    previousURL = null;
    fibiApplicationUrl = '';
    $ScrollAction = new Subject<{event: Event,pageYOffset: number}>();
    $sliderScrollAction = new Subject<{event: Event,pageYOffset: number}>();
    $commentConfigurationDetails =  new BehaviorSubject<any>({});
    enableGraph = false;
    $updateLatestNote = new Subject();
    $updateLatestAttachment = new Subject();
    isShowCreateNoteModal = false;
    isOpenAttachmentModal = false;

    constructor(private _http: HttpClient, private elasticConfigService: ElasticConfigService,private _router: Router) {
    }

    /**
     * returns a config file from assets and assign to application variables
     */
    async getAppConfig() {
        return new Promise(async (resolve, reject) => {
            const CONFIG_DATA: any = await this.readConfigFile();
            this.assignConfigurationValues(CONFIG_DATA);
            if (this.enableSSO) {
                const USER_DATA = await this.loginWithCurrentUser();
                this.isValidUser = USER_DATA.body['login'];
                this.updateLocalStorageWithUserDetails(USER_DATA);
            }
            if (this.currentUserDetails && this.currentUserDetails.Authorization) {
                try {
                    // const SYSTEM_PARAMETERS: any = await this.getRequiredParameters();
                    // this.assignSystemParameters(SYSTEM_PARAMETERS);
                    await this.fetchPermissions();
                    resolve(true);
                } catch (e) {
                    console.error(e);
                    resolve(true);
                }
            } else {
                resolve(true);
            }
        });
    }

    readConfigFile() {
        let headers: HttpHeaders = new HttpHeaders();
        headers = headers.append('Cache-Control', 'no-store');
        headers = headers.append('Pragma', 'no-cache');
        return this._http.get(environment.deployUrl + 'assets/app-config.json', {headers}).toPromise();
    }

    /**
     * @param  {} configurationData
     * assign system configurations to global variables
     */
    assignConfigurationValues(configurationData) {
        this.baseUrl = configurationData.baseUrl;
        this.fibiUrl = configurationData.fibiUrl;
        this.authUrl = configurationData.authUrl;
        this.formUrl = configurationData.formUrl;
        this.opaUrl = configurationData.opaUrl;
        this.formUrl = configurationData.formUrl;
        this.enableSSO = configurationData.enableSSO;
        this.isElasticAuthentiaction = configurationData.isElasticAuthentiaction;
        this.elasticUserName = configurationData.elasticUserName;
        this.elasticDelimiter = configurationData.elasticDelimiter;
        this.elasticPassword = configurationData.elasticPassword;
        this.elasticAuthScheme = configurationData.elasticAuthScheme;
        this.elasticConfigService.url = configurationData.elasticIndexUrl;
        this.fibiApplicationUrl = configurationData.fibiApplicationUrl;
        this.enableGraph = configurationData.enableGraph;
    }

    pageScroll(elementId) {
        const id = document.getElementById(elementId);
        if (id) {
            id.scrollIntoView({behavior: 'smooth'});
        }
    }

    _keyPress(event: any, patternType) {
        const pattern = patternType === 'date' ? /[0-9\+\-\/\ ]/ : /[0-9\a-zA-Z]/;
        if (!pattern.test(String.fromCharCode(event.charCode))) {
            event.preventDefault();
        }
    }

    loginWithCurrentUser() {
        return this._http.post(this.baseUrl + '/auth/login', {}, {observe: 'response'}).toPromise();
    }

    /**
     * @param  {} details update the local storage with application constant values
     *  will be moved to application context once SSO is stable
     */
    updateLocalStorageWithUserDetails(details) {
        details.body['Authorization'] = details.headers.get('authorization');
        this.currentUserDetails = details.body;
        setIntoLocalStorage(details.body);
    }

    getCurrentUserDetail(detailsKey: string) {
        return this.currentUserDetails && this.currentUserDetails[detailsKey] ?
            this.currentUserDetails[detailsKey] : this.updateCurrentUser(detailsKey);
    }

    updateCurrentUser(detailsKey: string) {
        this.currentUserDetails = getFromLocalStorage();
        return this.currentUserDetails && this.currentUserDetails[detailsKey] ? this.currentUserDetails[detailsKey] : '';
    }

    getDashboardActiveModules(moduleCode = '') {
        return this._http.get(this.baseUrl + '/getModulesConfiguration' + (moduleCode ? '/' + moduleCode : ''));
    }

    /**
     * Converts array to an object with keys as sectionCode or subSectionCodes and values as the whole object.
     * @param data
     * */
    getSectionCodeAsKeys(data: any) {
        return data.sectionConfig.reduce((acc, obj) => {
            const subSections = obj.subSectionConfig.reduce((ac, ob) => ({...ac, [ob.subSectionCode]: ob}), {});
            return {...acc, [obj.sectionCode]: obj, ...subSections};
        }, {});
    }

    getRequiredParameters() {
        return this._http.get(this.baseUrl + '/fetchRequiredParams').toPromise();
    }

    /**
     * @param  {} parameters assign system level parameters to global variables
     */
    assignSystemParameters(parameters) {
        this.isEvaluation = parameters.isEvaluation;
        this.isMapRouting = parameters.isMapRouting;
        this.isEvaluationAndMapRouting = parameters.isEvaluationAndMapRouting;
        if (parameters.fileTypes && parameters.fileTypes.length) {
            this.cvFileType = parameters.fileTypes[1] ? parameters.fileTypes[1].extension : null;
            this.claimFileType = parameters.fileTypes[2] ? parameters.fileTypes[2].extension : null;
        }
        this.isWafEnabled = parameters.isWafEnabled;
        this.canAddOrganization = parameters.canUserAddOrganization;
        this.isGrantCallStatusAutomated = parameters.isGrantCallStatusAutomated;
        this.isManpowerEnabled = parameters.isAwardManpowerActive;
        this.isEnableAddtoAddressBook = parameters.isEnableAddtoAddressBook;
        this.isDevProposalVersioningEnabled = parameters.isDevProposalVersioningEnabled;
        this.isCreateAgreement = parameters.isShowCreateAgreement;
        this.isShowAgreementNotifyAction = parameters.isShowAgreementNotifyAction;
        this.isShowAgreementSupport = parameters.isShowAgreementSupport;
        this.isEnableLock = parameters.isEnableLock;
    }

    async fetchPermissions(hardRefresh = false) {
        if (!hardRefresh && this.rightsArray.length) {
            return this.rightsArray;
        }
        const {fibiRights, coiRights} = await this.getAllSystemRights();
        this.assignFibiBasedRights(fibiRights);
        this.assignCOIBasedRights(coiRights);
        return this.rightsArray;
    }

    private assignCOIBasedRights(coiRights) {
        if (coiRights) {
            if ('IS_REVIEW_MEMBER' in coiRights) {
                this.isCoiReviewer = coiRights.IS_REVIEW_MEMBER;
            }
            if ('IS_OPA_REVIEW_MEMBER' in coiRights) {
                this.isOPAReviewer = coiRights.IS_OPA_REVIEW_MEMBER;
            }
            if (Array.isArray(coiRights.rights)) {
                this.rightsArray = [...this.rightsArray, ...coiRights.rights];
            }
        }
    }

    private assignFibiBasedRights(fibiRights) {
        if (fibiRights.length) {
            this.rightsArray = fibiRights;
        }
    }

    private async getAllSystemRights() {
        const fibiRightsAPI = this._http.get(this.fibiUrl + '/getAllSystemRights').toPromise();
        const coiRightsAPI = this._http.get(this.baseUrl + '/fetchAllCoiRights').toPromise();
        const [fibiRights, coiRights]: any = await Promise.all([fibiRightsAPI, coiRightsAPI]);
        return {fibiRights, coiRights};
    }

    showToast(status = HTTP_SUCCESS_STATUS, toastContent = '', timer = 5000) {
        const toast: any = new Toast(document.getElementById('coi-bootstrap-toast'));
        const toast_body: any = document.getElementById('coi-bootstrap-toast-body');
        this.appToastContent = toastContent === '' ? status === HTTP_SUCCESS_STATUS ?
            'Your details saved successfully' : 'Error Saving Data! Please try again' : toastContent;
        this.toastClass = status === HTTP_SUCCESS_STATUS ? 'bg-success' :'bg-danger';
        if (toast && toast_body) {
            ['bg-success', 'bg-danger'].forEach(className => toast._element.classList.remove(className));
            toast_body.innerText =  this.appToastContent;
            toast._element.classList.add(this.toastClass);
            toast.show();

            // Focus the toast element
            toast_body.focus();

            // Unfocus after 5000 milliseconds
            setTimeout(() => {
                toast_body.innerText = '';
                toast.hide();
            }, timer);
        }
    }

  getDisclosureConflictBadge(statusCode: string) {
        switch (String(statusCode)) {
            case '1':
                return 'green-badge';
            case '2':
            case '5':
                return 'brown-badge';
            case '3':
            case '6':
                return 'red-badge';
            case '4':
                return 'green-badge';
            default:
                return 'yellow-badge';
        }
    }

    getDisclosureConflictBadgeForSlider(statusCode: string) {
        switch (String(statusCode)) {
            case '1':
                return 'green-badge-for-slider';
            case '2':
            case '5':
                return 'brown-badge-for-slider';
            case '3':
            case '6':
                return 'red-badge-for-slider';
            case '4':
                return 'green-badge-for-slider';
            default:
                return 'yellow-badge-for-slider';
        }
    }

    getReviewStatusBadge(statusCode: string): string {
        switch (statusCode) {
            case '1':
                return 'yellow-badge';
            case '2':
                return 'blue-badge';
            case '3':
                return 'yellow-badge';
            case '4':
                return 'green-badge';
            case '5':
                return 'red-badge';
            case '6':
                return 'red-badge';
            case '7':
                return 'blue-badge';
            case '8':
                return 'green-badge';
            default:
                return 'red-badge';
        }
    }

    getDispositionStatusBadge(statusCode) {
        switch (statusCode) {
            case '1':
                return 'yellow-badge';
            case '2':
            case '4':
            case '5':
                return 'blue-badge';
            case '3':
            case '6':
                return 'green-badge';
            default:
                return 'yellow-badge';
        }
    }

    getTravelReviewStatusBadge(statusCode) {
        switch (statusCode) {
            case '1':
                return 'yellow-badge';
            case '2':
                return 'blue-badge';
            case '3':
                return 'green-badge';
            case '4':
                return 'orange-badge';
            case '5':
                return 'bright-red-badge';
            case '6':
            case '7':
                return 'green-badge';
            default:
                return 'red-badge';
        }
    }

    getDocumentStatusBadge(statusCode) {
        switch (statusCode) {
            case '1':
                return 'yellow-badge';
            case '2':
                return 'green-badge';
            default:
                return 'yellow-badge';
        }
    }

  getProjectDisclosureConflictStatusBadge(statusCode: string) {
    switch (String(statusCode)) {
        case '100':
            return 'green-badge';
        case '200':
            return 'brown-badge';
        case '300':
            return 'red-badge';
        case '400':
            return 'green-badge';
    }
}

getProjectDisclosureConflictStatusBadgeForConfiltSliderStyleRequierment(statusCode: string) {
    switch (String(statusCode)) {
        case '100':
            return 'green-badge-for-slider';
        case '200':
            return 'brown-badge-for-slider';
        case '300':
            return 'red-badge-for-slider';
        case '400':
            return 'green-badge-for-slider';
    }
}
    removeUserDetailsFromLocalStorage() {
        ['authKey', 'cookie', 'sessionId', 'currentTab'].forEach((item) => localStorage.removeItem(item));
    }

    getAvailableRight(rights: string | string[], method: Method = 'SOME'): boolean {
      const rightsArray = Array.isArray(rights) ? rights : [rights];
      if (method === 'EVERY') {
        return rightsArray.every((right) => this.rightsArray.includes(right));
      } else {
        return rightsArray.some((right) => this.rightsArray.includes(right));
      }
    }

    getPersonLeadUnitDetails(unitData: any): string {
        return getPersonLeadUnitDetails(unitData);
    }

    redirectionBasedOnRights() {
        this.fetchPermissions(true).then((res) => {
			const isAdministrator = this.getAvailableRight(['COI_ADMINISTRATOR', 'VIEW_ADMIN_GROUP_COI'])
				|| this.isCoiReviewer;
			const isOPAAdmin = this.getAvailableRight(['OPA_ADMINISTRATOR', 'VIEW_ADMIN_GROUP_OPA']);
			this._router.navigate([isAdministrator ? '/coi/admin-dashboard' : isOPAAdmin ? '/coi/opa-dashboard' : 'coi/user-dashboard']);
		});
    }

}
