import {Injectable} from '@angular/core';
import {HttpClient, HttpHeaders} from '@angular/common/http';
import {BehaviorSubject} from 'rxjs';
import {environment} from "../../../environments/environment";
import {getFromLocalStorage, setIntoLocalStorage} from "../../../../../fibi/src/app/common/utilities/user-service";
import {ElasticConfigService} from "../../../../../fibi/src/app/common/services/elastic-config.service";
import {Toast} from "bootstrap";
import { HTTP_SUCCESS_STATUS } from '../../app-constants';

@Injectable()
export class CommonService {

    isShowLoader = new BehaviorSubject<boolean>(true);
    isManualLoaderOn = false;
    isShowOverlay = false;
    baseUrl = '';
    fibiUrl = '';
    authUrl = '';
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

    constructor(private _http: HttpClient, private elasticConfigService: ElasticConfigService) {
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
                } catch (e) {
                    console.error(e)
                }
                resolve(true);
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
        this.enableSSO = configurationData.enableSSO;
        this.isElasticAuthentiaction = configurationData.isElasticAuthentiaction;
        this.elasticUserName = configurationData.elasticUserName;
        this.elasticDelimiter = configurationData.elasticDelimiter;
        this.elasticPassword = configurationData.elasticPassword;
        this.elasticAuthScheme = configurationData.elasticAuthScheme;
        this.elasticConfigService.url = configurationData.elasticIndexUrl;
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

    async fetchPermissions() {
        if (this.rightsArray.length) {
            return this.rightsArray;
        }
        this.rightsArray = this._http.get(this.baseUrl + '/getAllSystemRights').toPromise();
        return this.rightsArray;
    }

    showToast(status = HTTP_SUCCESS_STATUS, toastContent = '') {
        let toast: any = new Toast(document.getElementById('coi-bootstrap-toast'));
        let toast_body: any = document.getElementById('coi-bootstrap-toast-body');
        this.appToastContent = toastContent === '' ? status === HTTP_SUCCESS_STATUS ?
            'Your details saved successfully' : 'Error Saving Data! Please try again' : toastContent;
        this.toastClass = status === HTTP_SUCCESS_STATUS ? 'bg-success' : 'bg-danger';
        toast_body.innerText =  this.appToastContent;
        toast._element.classList.add(this.toastClass)
        toast.show();
    }

    getDisclosureConflictBadge(statusCode: string) {
        switch (String(statusCode)) {
            case '1':
                return 'bg-success text-white';
            case '2':
                return 'bg-warning text-black'
            case '3':
                return 'bg-danger text-white';
        }
    }

    getReviewStatusBadge(statusCode) {
        switch (statusCode) {
            case '1':
                return 'bg-warning text-black';
            case '2':
                return 'bg-info text-white';
            case '3':
                return 'bg-success text-white';
            case '4':
                return 'bg-success text-white';
            default:
                return 'bg-danger text-white';
        }
    }

    getDispositionStatusBadge(statusCode) {
        switch (statusCode) {
            case '1':
                return 'bg-warning text-black';
            case '2':
            case '4':
            case '5':
                return 'bg-info text-white';
            case '3':
            case '6':
                return 'bg-success text-white';
            default:
                return 'bg-warning text-black';
        }
    }

}
