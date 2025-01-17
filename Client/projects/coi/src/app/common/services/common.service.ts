import {Injectable} from '@angular/core';
import {HttpClient, HttpHeaders} from '@angular/common/http';
import {BehaviorSubject, Subject} from 'rxjs';
import {environment} from '../../../environments/environment';
import {getFromLocalStorage, setIntoLocalStorage} from '../../../../../fibi/src/app/common/utilities/user-service';
import {Toast} from 'bootstrap';
import { HTTP_SUCCESS_STATUS, AWARD_EXTERNAL_RESOURCE_URL, PROPOSAL_EXTERNAL_RESOURCE_URL, IP_EXTERNAL_RESOURCE_URL, ADMIN_DASHBOARD_RIGHTS, COI_DISCLOSURE_SUPER_ADMIN_RIGHTS, HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG, OPA_DISCLOSURE_RIGHTS, OPA_DISCLOSURE_ADMIN_RIGHTS, PERSON_EXTERNAL_RESOURCE_URL, ROLODEX_PERSON_EXTERNAL_RESOURCE_URL } from '../../app-constants';
import { getPersonLeadUnitDetails } from '../utilities/custom-utilities';
import { Router } from '@angular/router';
import { ElasticConfigService } from './elastic-config.service';
import { DisclosureProjectData, DisclosureProjectModalData } from '../../shared-components/shared-interface';
import { LoginPersonDetails, GlobalEventNotifier, LoginPersonDetailsKey, Method, CoiAttachmentModalInfo, COIAppConfig } from './coi-common.interface';
import { AttachmentInputType, COIAttachment } from '../../attachments/attachment-interface';
import { hideModal } from "../../../../../fibi/src/app/common/utilities/custom-utilities";
import { ProjectHierarchySliderPayload } from '../../shared-components/project-hierarchy-slider/services/project-hierarchy-slider.interface';
import { NotificationTypeRO } from '../../admin-dashboard/admin-dashboard.interface';
import { removeToast } from '../../entity-management-module/shared/entity-interface';

@Injectable()
export class CommonService {

    isShowLoader = new BehaviorSubject<boolean>(true);
    baseUrl = '';
    fibiUrl = '';
    authUrl = '';
    opaUrl = '';
    formUrl = '';
    fibiCOIConnectUrl = '';
    EXTERNAL_APPLICATION_BASE_URL = '';
    EXTERNAL_DEV_PROPOSAL_URL = '';
    EXTERNAL_PERSON_URL = '';
    EXTERNAL_ROLODEX_PERSON_URL = '';
    EXTERNAL_AWARD_URL = '';
    EXTERNAL_IP_URL = '';
    currencyFormat = '$';
    forbiddenModule = '';
    isEvaluation: boolean;
    isMapRouting: boolean;
    isEvaluationAndMapRouting: boolean;
    cvFileType: any = [];
    wordFileType: any = [];
    claimFileType: any = [];
    enableSSO = false;
    rightsArray: any = [];
    isIE = /msie\s|trident\//i.test(window.navigator.userAgent);
    extension: any = [];
    currentUserDetails = new LoginPersonDetails();
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
    generalFileType: any = [];
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
    projectDetailsModalInfo = new DisclosureProjectModalData();
    modalPersonId: string = '';
    $globalEventNotifier = new Subject<GlobalEventNotifier>();
    relationshipTypeCache = {};
    entityURL: any;
    hasChangesAvailable = false;
    isNavigationStopped: boolean = false;
    attemptedPath: string = '';
    CoiAttachmentModalInfo = new CoiAttachmentModalInfo();
    isEntityModified = false;
    projectHierarchySliderInfo = new ProjectHierarchySliderPayload();
    autoSaveSavingLoader: 'SHOW'|'HIDE' = 'HIDE';
    loaderRestrictedUrls: any[] = [];

    constructor(private _http: HttpClient, private elasticConfigService: ElasticConfigService, private _router: Router) {
    }

    /**
     * returns a config file from assets and assign to application variables
     */
    async getAppConfig() {
        return new Promise(async (resolve, reject) => {
            const CONFIG_DATA: COIAppConfig = await this.readConfigFile();
            this.assignConfigurationValues(CONFIG_DATA);
            try {
                const loginUserDetails: any = await this.authLogin();
                this.onSuccessFullLogin(loginUserDetails);
                const SYSTEM_PARAMETERS: any = await this.getRequiredParameters();
                this.assignSystemParameters(SYSTEM_PARAMETERS);
                resolve(true);
            } catch (e) {
                this.onFailedLogin(e);
                resolve(true);
            }
        });
    }

    readConfigFile(): Promise<any> {
        let headers: HttpHeaders = new HttpHeaders();
        headers = headers.append('Cache-Control', 'no-store');
        headers = headers.append('Pragma', 'no-cache');
        return this._http.get(environment.deployUrl + 'assets/app-config.json', {headers}).toPromise();
    }

    /**
     * @param  {} configurationData
     * assign system configurations to global variables
     */
    assignConfigurationValues(configurationData: COIAppConfig) {
        this.baseUrl = configurationData.baseUrl;
        this.fibiUrl = configurationData.fibiUrl;
        this.authUrl = configurationData.authUrl;
        this.formUrl = configurationData.formUrl;
        this.opaUrl = configurationData.opaUrl;
        this.fibiCOIConnectUrl = configurationData.fibiCOIConnectUrl;
        this.enableSSO = configurationData.enableSSO;
        this.isElasticAuthentiaction = configurationData.isElasticAuthentiaction;
        this.elasticUserName = configurationData.elasticUserName;
        this.elasticDelimiter = configurationData.elasticDelimiter;
        this.elasticPassword = configurationData.elasticPassword;
        this.elasticAuthScheme = configurationData.elasticAuthScheme;
        this.elasticConfigService.url = configurationData.elasticIndexUrl;
        this.elasticConfigService.indexValue = configurationData.indexValue;
        this.fibiApplicationUrl = configurationData.fibiApplicationUrl;
        this.enableGraph = configurationData.enableGraph;
        this.EXTERNAL_APPLICATION_BASE_URL = configurationData.EXTERNAL_APPLICATION_BASE_URL;
        this.EXTERNAL_DEV_PROPOSAL_URL = configurationData.EXTERNAL_DEV_PROPOSAL_URL;
        this.EXTERNAL_AWARD_URL = configurationData.EXTERNAL_AWARD_URL;
        this.EXTERNAL_IP_URL = configurationData.EXTERNAL_IP_URL;
        this.EXTERNAL_PERSON_URL = configurationData.EXTERNAL_PERSON_URL;
        this.EXTERNAL_ROLODEX_PERSON_URL = configurationData.EXTERNAL_ROLODEX_PERSON_URL;
        this.entityURL = configurationData.entityURL;
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
        return this._http.post(this.formUrl + '/auth/login', {}, {observe: 'response'}).toPromise();
    }

    authLogin() {
        return this._http.post(this.authUrl + '/login', {}, {observe: 'response'}).toPromise();
    }

    signOut() {
        return this._http.get(this.authUrl + '/logout');
    }

    /**
     * @param  {} details update the local storage with application constant values
     *  will be moved to application context once SSO is stable
     */
    updateLocalStorageWithUserDetails(details: any) {
        this.currentUserDetails = details.body;
        setIntoLocalStorage(details.body);
    }

    getCurrentUserDetail(detailsKey: LoginPersonDetailsKey) {
        return this.currentUserDetails && this.currentUserDetails[detailsKey] ?
            this.currentUserDetails[detailsKey] : this.updateCurrentUser(detailsKey);
    }

    updateCurrentUser(detailsKey: LoginPersonDetailsKey) {
        this.currentUserDetails = getFromLocalStorage();
        return this.currentUserDetails && this.currentUserDetails[detailsKey] ? this.currentUserDetails[detailsKey] : '';
    }

    getDashboardActiveModules(moduleCode = '') {
        return this._http.get(this.fibiUrl + '/getModulesConfiguration' + (moduleCode ? '/' + moduleCode : ''));
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
        if (parameters.fileTypes?.length) {
            this.generalFileType = parameters.fileTypes[0] ? parameters.fileTypes[0].extension : null;
            this.cvFileType = parameters.fileTypes[1] ? parameters.fileTypes[1].extension : null;
            this.claimFileType = parameters.fileTypes[2] ? parameters.fileTypes[2].extension : null;
            this.wordFileType = parameters.fileTypes[3] ? parameters.fileTypes[3].extension : null;
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
    }

    assignCOIBasedRights(coiRights) {
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

    assignFibiBasedRights(fibiRights) {
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

    getConsultingDispositionStatusBadge(statusCode) {
        switch (statusCode) {
            case '1':
                return 'yellow-badge';
            case '2':
                return 'green-badge';
            default:
                return '';
        }
    }

    getConsultingReviewStatusBadge(statusCode: string): string {
        switch (statusCode) {
            case '1':
                return 'yellow-badge';
            case '2':
                return 'blue-badge';
            case '3':
                return 'yellow-badge';
            case '5':
                return 'green-badge';
            case '8':
                return 'red-badge';
            case '7':
                return 'red-badge';
            case '4':
                return 'blue-badge';
            case '6':
                return 'green-badge';
            default:
                return '';
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

    onSuccessFullLogin(userData) {
        this.updateLocalStorageWithUserDetails(userData);
        this.refreshAfterLogin();
    }

    /**
     * Redirects to project details based on the project type.
     *
     * @param projectTypeCode - The moduleCode/typeCode of project (1: Award, 2: Institute Proposal, 3: Development Proposal).
     * @param projectId - The unique ID of the project to redirect to.
     *
     * Project Types:
     * 1: Award - Uses EXTERNAL_AWARD_URL (if defined) or falls back to AWARD_EXTERNAL_RESOURCE_URL for FIBI integration.
     * 2: Institute Proposal - Uses EXTERNAL_IP_URL (if defined) or falls back to IP_EXTERNAL_RESOURCE_URL for FIBI integration.
     * 3: Development Proposal - Uses EXTERNAL_DEV_PROPOSAL_URL (if defined) or falls back to PROPOSAL_EXTERNAL_RESOURCE_URL for FIBI integration.
     *
     * External URLs:
     * EXTERNAL_AWARD_URL, EXTERNAL_IP_URL, EXTERNAL_DEV_PROPOSAL_URL, EXTERNAL_APPLICATION_BASE_URL => for KC integration or external integration.
     * AWARD_EXTERNAL_RESOURCE_URL, IP_EXTERNAL_RESOURCE_URL, PROPOSAL_EXTERNAL_RESOURCE_URL, fibiApplicationUrl => for FIBI.
     */
    redirectToProjectDetails(projectTypeCode: string | number, projectId: string): void {
        const EXTERNAL_PROJECT_URLS = [this.EXTERNAL_AWARD_URL, this.EXTERNAL_IP_URL, this.EXTERNAL_DEV_PROPOSAL_URL];
        const PROJECT_URLS_MAP = {
            1: EXTERNAL_PROJECT_URLS[0] || AWARD_EXTERNAL_RESOURCE_URL,
            2: EXTERNAL_PROJECT_URLS[1] || IP_EXTERNAL_RESOURCE_URL,
            3: EXTERNAL_PROJECT_URLS[2] || PROPOSAL_EXTERNAL_RESOURCE_URL,
        };
        if (PROJECT_URLS_MAP[projectTypeCode]) {
            const IS_EXTERNAL_URL = EXTERNAL_PROJECT_URLS.includes(PROJECT_URLS_MAP[projectTypeCode]);
            const BASE_URL = IS_EXTERNAL_URL ? this.EXTERNAL_APPLICATION_BASE_URL : this.fibiApplicationUrl;
            const RESOURCE_URL = PROJECT_URLS_MAP[projectTypeCode].replace('{projectId}', projectId);
            window.open(`${BASE_URL}${RESOURCE_URL}`, '_blank');
        }
    }

    /**
     * Redirects to person details based on the personType ('PERSON' or 'ROLODEX').
     *
     * @param personId - The unique ID of the person to redirect to.
     * @param personType - Specifies if the person is a 'PERSON' (default) or 'ROLODEX'.
     *
     * PERSON:
     *  - Uses EXTERNAL_PERSON_URL (if defined) or falls back to PERSON_EXTERNAL_RESOURCE_URL for FIBI integration.
     *  - Represents standard users or individuals directly interacting with the application.
     * ROLODEX:
     *  - Uses EXTERNAL_ROLODEX_PERSON_URL (if defined) or falls back to ROLODEX_PERSON_EXTERNAL_RESOURCE_URL for KC integration or external integration.
     *  - Represents contacts stored in a rolodex-like directory, typically external users / not standard users of the application.
     * BASE_URL:
     *  - Uses EXTERNAL_APPLICATION_BASE_URL if the URL is external; otherwise, it uses fibiApplicationUrl for FIBI redirection.
     */
    redirectToPersonDetails(personId: string, personType: 'PERSON' | 'ROLODEX' = 'PERSON'): void {
        const EXTERNAL_PERSON_URLS = [this.EXTERNAL_PERSON_URL, this.EXTERNAL_ROLODEX_PERSON_URL];
        const PERSON_URLS_MAP = {
            'PERSON': EXTERNAL_PERSON_URLS[0] || PERSON_EXTERNAL_RESOURCE_URL,
            'ROLODEX': EXTERNAL_PERSON_URLS[1] || ROLODEX_PERSON_EXTERNAL_RESOURCE_URL
        };
        if (PERSON_URLS_MAP[personType]) {
            const IS_EXTERNAL_URL = EXTERNAL_PERSON_URLS.includes(PERSON_URLS_MAP[personType]);
            const BASE_URL = IS_EXTERNAL_URL ? this.EXTERNAL_APPLICATION_BASE_URL : this.fibiApplicationUrl;
            const RESOURCE_URL = PERSON_URLS_MAP[personType].replace('{personId}', personId);
            window.open(`${BASE_URL}${RESOURCE_URL}`, '_blank');
        }
    }

    setLoaderRestriction(): void {
        this.isPreventDefaultLoader = true;
    }

    removeLoaderRestriction(): void {
        this.isPreventDefaultLoader = false;
    }

    openProjectDetailsModal(projectDetails: DisclosureProjectData | null = null, coiDisclosureId: number | null = null, needReporterRole: boolean = true): void {
        this.projectDetailsModalInfo.projectDetails = projectDetails;
        this.projectDetailsModalInfo.coiDisclosureId = coiDisclosureId;
        this.projectDetailsModalInfo.needReporterRole = needReporterRole;
    }

    closeProjectDetailsModal(isOpen = true): void {
        if (isOpen) {
            document.getElementById('coi-project-view-modal-close-btn')?.click();
        }
        setTimeout(() => {
            this.projectDetailsModalInfo = new DisclosureProjectModalData();
        }, 200);
    }

    openProjectHierarchySlider(projectTypeCode: string | number | null, projectNumber: string | null): void {
        if (projectTypeCode && projectNumber) {
            this.projectHierarchySliderInfo.projectTypeCode = projectTypeCode;
            this.projectHierarchySliderInfo.projectNumber = projectNumber;
            this.projectHierarchySliderInfo.isOpenSlider = true;
        } else {
            this.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
        }
    }

    closeProjectHierarchySlider(): void {
        this.projectHierarchySliderInfo = new ProjectHierarchySliderPayload();
    }

    openPersonDetailsModal(personId: string): void {
        this.modalPersonId = personId;
    }

    closePersonDetailsModal(isOpen = true): void {
        if (isOpen) {
            document.getElementById('coi-person-view-modal-close-btn')?.click();
        }
        setTimeout(() => {
            this.modalPersonId ='';
        }, 200);
    }

    getRiskColor(typeCode: string): string {
        switch (typeCode) {
            case '1':
                return 'high-risk';
            case '2':
                return 'medium-risk';
            case '3':
                return 'low-risk';
            default:
                return '';
        }
    }

    getEntityRelationTypePills(validPersonEntityRelType: string) {
        if (validPersonEntityRelType) {
            if (this.relationshipTypeCache[validPersonEntityRelType]) {
                return this.relationshipTypeCache[validPersonEntityRelType];
            }
            const entityRelTypes = validPersonEntityRelType.split(':;:');
            this.relationshipTypeCache[validPersonEntityRelType] = entityRelTypes.map(entity => {
                const relationshipType = entity.split(':');
                return { relationshipType: relationshipType[0] || '', description: relationshipType[1] || '' };
            });
            return this.relationshipTypeCache[validPersonEntityRelType];
        }
    }

    getRelationshipIcon(key: string): string {
        switch (key) {
            case 'Commitment': return 'handshake';
            case 'Travel': return 'flight';
            case 'Financial': return 'paid';
            case 'Consulting': return 'supervisor_account';
            default: return;
        }
    }

    //There are some scenarios where we have to refresh and call get login user detials and navigate similar to initail login.
    //when we are in login page itself , or logout page we have to refresh and navigate to page based on rights.
    //during 403, 401 also need right based navigation.
    refreshAfterLogin() {
        const currentUrl = window.location.href;
        let PATH_STR = ['login', 'logout', '401', '403'];
        if (PATH_STR.some((str) => currentUrl.includes(str))) {
            this.redirectionBasedOnRights();
        }
    }

    onFailedLogin(err): void {
        if (err.status == 401) {
            this.enableSSO ? window.location.reload() : this._router.navigate(['/login']);
        } else if (err.status === 403) {
            this._router.navigate(['/error-handler/403']);
        }
    }

    checkFCOIRights(): boolean {
        return this.getAvailableRight(COI_DISCLOSURE_SUPER_ADMIN_RIGHTS) || this.isCoiReviewer || this.getAvailableRight(ADMIN_DASHBOARD_RIGHTS);
    }

    checkOPARights(): boolean {
        return this.getAvailableRight(OPA_DISCLOSURE_ADMIN_RIGHTS) || this.isOPAReviewer || this.getAvailableRight(OPA_DISCLOSURE_RIGHTS);
    }

    redirectionBasedOnRights(): void {
        this.fetchPermissions(true).then((res) => {
            const CAN_VIEW_FCOI_DASHBOARD  = this.checkFCOIRights();
            const CAN_VIEW_OPA_DASHBOARD = this.checkOPARights();
            this._router.navigate([CAN_VIEW_FCOI_DASHBOARD  ? '/coi/admin-dashboard' : CAN_VIEW_OPA_DASHBOARD ? '/coi/opa-dashboard' : 'coi/user-dashboard']);
        });
    }

    removeUserDetailsFromLocalStorage() {
        ['authKey', 'cookie', 'sessionId', 'currentTab'].forEach((item) => localStorage.removeItem(item));
    }

    fetchAllNotifications(notificationRequest: NotificationTypeRO) {
        return this._http.post(this.fibiUrl + '/getNotifications', notificationRequest);
    }

    getSectionName(tabName, section) {
        const sectionDetails = tabName.get(section);
        if (sectionDetails) {
           return sectionDetails.sectionName;
        }
   }

   getSectionId(tabName, section) {
       const sectionDetails = tabName.get(section);
       if (sectionDetails) {
           return sectionDetails.sectionId;
       }
   }

   getSubSectionId(tabName, section) {
       const sectionDetails = tabName.get(section);
       if (sectionDetails) {
           return sectionDetails.subSectionId;
       }
   }

    openCommonAttachmentModal(attachmentInputType: AttachmentInputType, currentAttachment: COIAttachment = null): void {
        this.CoiAttachmentModalInfo = {
            attachmentModalInputType: attachmentInputType,
            coiCurrentAttachment: currentAttachment,
            isOpenAttachmentModal: true,
        }
    }

    closeCommonAttachmentModal(): void {
        this.CoiAttachmentModalInfo = new CoiAttachmentModalInfo();
    }

    setChangesAvailable(hasChange: boolean) {
        this.hasChangesAvailable = hasChange;
        if (!hasChange) {
            this.navigateToRoute();
        }
    }

    navigateToRoute() {
        if (this.isNavigationStopped) {
            hideModal('coi-entity-confirmation-modal');
            this._router.navigateByUrl(this.attemptedPath);
        }
    }

    showAutoSaveLoader(): void {
        removeToast('SUCCESS');
        removeToast('ERROR');
        this.autoSaveSavingLoader = 'SHOW';
    }

}
