import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { TravelDisclosureService } from './services/travel-disclosure.service';
import { CommonService } from '../common/services/common.service';
import { Subscription } from 'rxjs';
import { SfiService } from '../disclosure/sfi/sfi.service';
import { subscriptionHandler } from '../../../../fibi/src/app/common/utilities/subscription-handler';
import { environment } from '../../environments/environment';
import { TravelDataStoreService } from './services/travel-data-store.service';
import {
    CoiTravelDisclosure, TravelCreateModalDetails,
    TravelActionAfterSubmitRO, TravelDisclosure, EntityDetails, ModalSize, 
    TabType} from './travel-disclosure.interface';
import {
    REPORTER_HOME_URL, HTTP_ERROR_STATUS, ADMIN_DASHBOARD_URL, HTTP_SUCCESS_STATUS,
    CREATE_TRAVEL_DISCLOSURE_ROUTE_URL, POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL, TRAVEL_REVIEW_STATUS
} from '../app-constants';
import { NavigationService } from '../common/services/navigation.service';
import { DefaultAssignAdminDetails, PersonProjectOrEntity } from '../shared-components/shared-interface';
import { closeCommonModal, openCommonModal } from '../common/utilities/custom-utilities';
import { heightAnimation } from '../common/utilities/animations';
import { GlobalEventNotifier } from '../common/services/coi-common.interface';

type Method = 'SOME' | 'EVERY';

@Component({
    selector: 'app-travel-disclosure',
    templateUrl: './travel-disclosure.component.html',
    styleUrls: ['./travel-disclosure.component.scss'],
    animations: [heightAnimation('0', '*', 300, 'heightAnimation')]
})

export class TravelDisclosureComponent implements OnInit, OnDestroy {

    modalSize: ModalSize;
    deployMap = environment.deployUrl;
    $subscriptions: Subscription[] = [];
    travelDisclosure = new TravelDisclosure();
    TRAVEL_REVIEW_STATUS = TRAVEL_REVIEW_STATUS;
    personEntityDetails = new PersonProjectOrEntity();
    defaultAdminDetails = new DefaultAssignAdminDetails();
    newTravelDisclosureCreateObject = new TravelCreateModalDetails();

    isCreateMode = true;
    isMandatory = false;
    isOpenSlider = false;
    isCardExpanded = true;
    isOpenRiskSlider = false;
    isShowPersonSlider = false;
    isAddAssignModalOpen = false;
    needDescriptionField = false;
    isTravelAdministrator = false;
    isShowPersonEntityDetails = false;

    reasonHelpText = '';
    currentPersonId = '';
    modalHeaderTitle = '';
    textAreaLabelName = '';
    modalActionBtnName = '';
    descriptionErrorMsg = '';
    selectedPersonSliderType = '';
    confirmationModalHelpText = '';
    confirmationModalDescription = '';
    returnErrorMsg = 'Please provide the reason for returning the disclosure.';
    withdrawErrorMsg = 'Please provide the reason for withdrawing the disclosure.';
    userDetails = {
        fullName: '',
        personId: '',
        homeUnit: null,
        homeUnitName: null,
        emailAddress: '',
        primaryTitle: ''
    };

    constructor(
        public router: Router,
        public sfiService: SfiService,
        private _route: ActivatedRoute,
        public commonService: CommonService,
        public service: TravelDisclosureService,
        private _dataStore: TravelDataStoreService,
        private _navigationService: NavigationService) {
    }

    ngOnInit(): void {
        this.fetchTravelRights();
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.listenQueryParamsChanges();
        this.listenGlobalEventNotifier();
        this.isCreateMode = this._dataStore.getEditModeForDisclosure();
    }

    ngOnDestroy(): void {
        this.clearAllDetails();
    }

    private fetchTravelRights(): void {
        this.currentPersonId = this.commonService.getCurrentUserDetail('personID');
        this.isTravelAdministrator = this.commonService.getAvailableRight('MANAGE_TRAVEL_DISCLOSURE');
    }

    private listenQueryParamsChanges(): void {
        this.$subscriptions.push(this._route.queryParams.subscribe(params => {
            const MODULE_ID = params['disclosureId'];
            const homeUnit = this.getHomeUnit();
            if (MODULE_ID && this.travelDisclosure.travelDisclosureId != MODULE_ID) {
                this.loadNewTravelDisclosureAndUpdateStore(MODULE_ID);
            }
            if (!MODULE_ID) {
                this.resetTravelDisclosure();             
            }
            if (!homeUnit && !MODULE_ID) {
                this.router.navigate([REPORTER_HOME_URL]);
            }
        }));
    }

    private loadNewTravelDisclosureAndUpdateStore(travelDisclosureId: number) {
        this.fetchTravelRights();
        this.$subscriptions.push(this.service.loadTravelDisclosure(travelDisclosureId).subscribe((data: any) => {
            this._dataStore.setStoreData(data);
        }));
    }

    private listenGlobalEventNotifier(): void {
        this.$subscriptions.push(
            this.commonService.$globalEventNotifier.subscribe((event: GlobalEventNotifier) => {
                if (event.uniqueId === 'CREATE_NEW_TRAVEL_DISCLOSURE') {
                    this.service.isCreateNewTravelDisclosure = true;
                    this.newTravelDisclosureCreateObject = event.content;
                    this.service.travelDataChanged ? openCommonModal('travel-unsaved-changes-modal') : this.redirectBasedOnCreateDisclosure();
                }
            })
        );
    }

    private resetTravelDisclosure(): void {
        this.setCreateModalDetails();
        this.travelDisclosure = new TravelDisclosure();
        this._dataStore.setStoreData(this.travelDisclosure);
        this.setUserDetails();
        this.service.setUnSavedChanges(false, '');
        this.service.isCreateNewTravelDisclosure = false;
    }

    private getHomeUnit(): string | null {
        const travelCreateModalDetails = this._dataStore.getCreateModalDetails();
        if (travelCreateModalDetails?.homeUnit) {
            return travelCreateModalDetails.homeUnit;
        }
        return null;
    }

    private clearAllDetails(): void {
        this.travelDisclosure = new TravelDisclosure();
        this._dataStore.setStoreData(this.travelDisclosure);
        this.removeSessionStorage();
        this.service.setUnSavedChanges(false, '');
        this.service.travelEntityDetails = new EntityDetails();
        subscriptionHandler(this.$subscriptions);
    }

    private getDataFromStore(): void {
        this.service.isAllowNavigation = false;
        if (this._dataStore.getData().travelDisclosureId) {
            this.travelDisclosure = this._dataStore.getData();
            this.userDetails.personId = this.travelDisclosure.personId;
            this.userDetails.fullName = this.travelDisclosure.personFullName;
            this.userDetails.homeUnit = this.travelDisclosure.homeUnitNumber;
            this.userDetails.homeUnitName = this.travelDisclosure.homeUnitName;
            this.userDetails.emailAddress = this.travelDisclosure.personEmail;
            this.userDetails.primaryTitle = this.travelDisclosure.personPrimaryTitle;
            this.service.travelEntityDetails = this._dataStore.getEntityDetails();
            this.setPersonEntityDetails();
        } else {
            this.setUserDetails();
        }
    }

    private setAssignAdminModalDetails(): void {
        this.defaultAdminDetails.adminGroupId = this.travelDisclosure.adminGroupId;
        this.defaultAdminDetails.adminPersonId = this.travelDisclosure.adminPersonId;
        this.defaultAdminDetails.adminGroupName = this.travelDisclosure.adminGroupName;
        this.defaultAdminDetails.adminPersonName = this.travelDisclosure.adminPersonName;
    }

    private setModalHeaderTitle(modalActionBtnName: string): void {
        const id = this.travelDisclosure.travelDisclosureId || null;
        const creatorName = this.travelDisclosure.personFullName || null;
        const btnName = modalActionBtnName;

        if (id) {
            this.modalHeaderTitle = `${btnName} Disclosure #${id} : Travel Disclosure By ${creatorName}`;
        } else {
            this.modalHeaderTitle = `Travel Disclosure By ${creatorName}`;
        }
    }

    private setUserDetails(): void {
        const travelCreateModalDetails: TravelCreateModalDetails = this._dataStore.getCreateModalDetails();
        this.userDetails.fullName = this.commonService.getCurrentUserDetail('fullName');
        this.userDetails.homeUnitName = travelCreateModalDetails?.homeUnitName;
        this.userDetails.personId = travelCreateModalDetails?.personId;
        this.userDetails.homeUnit = travelCreateModalDetails?.homeUnit;
        this.userDetails.primaryTitle = this.commonService.getCurrentUserDetail('primaryTitle');
        this.userDetails.emailAddress = this.commonService.getCurrentUserDetail('email');
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private setApprovalChangesToDisclosure(travelDisclosure): void {
        this.travelDisclosure.reviewStatus = travelDisclosure.reviewStatus;
        this.travelDisclosure.acknowledgeAt = travelDisclosure.acknowledgeAt;
        this.travelDisclosure.acknowledgeBy = travelDisclosure.acknowledgeBy;
        this.travelDisclosure.documentStatus = travelDisclosure.documentStatus;
        this.travelDisclosure.updateTimestamp = travelDisclosure.updateTimestamp;
        this.travelDisclosure.reviewStatusCode = travelDisclosure.reviewStatusCode;
        this.travelDisclosure.documentStatusCode = travelDisclosure.documentStatusCode;
        this._dataStore.manualDataUpdate(this.travelDisclosure);
    }

    private getActionRequestObject(): TravelActionAfterSubmitRO {
        return {
            travelDisclosureId: this.travelDisclosure.travelDisclosureId,
            description: this.confirmationModalDescription
        };
    }

    private reRoutePage(): void {
        if (!this.service.isCheckLoggedUser(this.travelDisclosure.personId)) {
            this.navigateBack();
        } else {
            this.router.navigate([CREATE_TRAVEL_DISCLOSURE_ROUTE_URL],
                { queryParams: { disclosureId: this.travelDisclosure.travelDisclosureId } });
        }
    }

    private navigateBack(): void {
        const PREVIOUS_MODULE_URL = this.service.PREVIOUS_MODULE_URL;
        const ROUTE_URL = this.service.isAdminDashboard ? ADMIN_DASHBOARD_URL : REPORTER_HOME_URL;
        (PREVIOUS_MODULE_URL?.includes('travel-disclosure')) ?
            this.router.navigate([ROUTE_URL]) : this.router.navigateByUrl(PREVIOUS_MODULE_URL || ROUTE_URL);
    }

    isRouteComplete(possibleActiveRoutes: string[] = []): boolean {
        return possibleActiveRoutes.some(paths => this.router.url.includes(paths));
    }

    checkReviewStatusCode(statusCode: string[] | string, method: Method = 'EVERY'): boolean {
        const statusArray = Array.isArray(statusCode) ? statusCode : [statusCode];
        if (method === 'EVERY') {
            return statusArray.every((status) => status.includes(this.travelDisclosure.reviewStatusCode));
        } else {
            return statusArray.some((status) => status.includes(this.travelDisclosure.reviewStatusCode));
        }
    }

    checkAdministratorRight(): boolean {
        return this.service.isCheckLoggedUser(this.travelDisclosure.adminPersonId);
    }

    showReturnOrApproveButton(): boolean {
        return this.checkAdministratorRight() && this.checkReviewStatusCode([TRAVEL_REVIEW_STATUS.REVIEW_IN_PROGRESS]);
    }

    openAddAssignModal(): void {
        this.isAddAssignModalOpen = true;
        this.setAssignAdminModalDetails();
    }

    closeAssignAdministratorModal(event): void {
        if (event && (event.adminPersonId || event.adminGroupId)) {
            this.travelDisclosure.adminPersonId = event.adminPersonId || null;
            this.travelDisclosure.adminPersonName = event.adminPersonName || null;
            this.travelDisclosure.adminGroupId = event.adminGroupId || null;
            this.travelDisclosure.adminGroupName = event.adminGroupName || null;
            this.travelDisclosure.reviewStatusCode = event.reviewStatusCode;
            this.travelDisclosure.reviewStatus = event.reviewStatus;
            this.travelDisclosure.updateTimestamp = event.updateTimestamp;
            this.travelDisclosure.disclosureStatusCode = event.disclosureStatusCode;
            this.travelDisclosure.disclosureStatus = event.disclosureStatus;
            this._dataStore.manualDataUpdate(this.travelDisclosure);
        }
        this.isAddAssignModalOpen = false;
    }

    openConfirmationModal(actionBtnName: string, needDescriptionField: boolean, isMandatory: boolean = false, descriptionErrorMsg: string = ''): void {
        this.modalActionBtnName = actionBtnName;
        this.needDescriptionField = needDescriptionField;
        this.isMandatory = isMandatory;
        this.textAreaLabelName = actionBtnName === 'Withdraw' ? ' Withdrawal' : (actionBtnName === 'Approve' ? 'Approval' : actionBtnName);
        this.modalSize = 'lg';
        this.setModalHeaderTitle(actionBtnName);
        this.descriptionErrorMsg = descriptionErrorMsg;
        this.confirmationModalHelpText = '';
        this.reasonHelpText = '';
        setTimeout(() => {
            this.reasonHelpText = `Please provide the reason for ${this.textAreaLabelName.toLowerCase()}.`;
            this.confirmationModalHelpText = `You are about to ${actionBtnName.toLowerCase()} the travel disclosure.`;
        });
        openCommonModal('travel-confirmation-modal');
    }

    private setPersonEntityDetails(): void {
        this.personEntityDetails.personFullName = this.travelDisclosure?.personFullName;
        this.personEntityDetails.entityName = this.travelDisclosure?.travelEntityName;
        this.personEntityDetails.homeUnitName = this.travelDisclosure?.homeUnitName;
        this.personEntityDetails.homeUnit = this.travelDisclosure?.homeUnitNumber;
        this.personEntityDetails.personEmail = this.travelDisclosure.personEmail;
        this.personEntityDetails.personPrimaryTitle = this.travelDisclosure.personPrimaryTitle;
        this.isShowPersonEntityDetails = true;
    }

    openConflictSlider(): void {
        this.isOpenSlider = true;
    }

    closeConflictSlider(): void {
        this.isOpenSlider = false;
    }


    performDisclosureAction(event: string): void {
        this.needDescriptionField = false;
        this.confirmationModalDescription = event;
        switch (this.modalActionBtnName) {
            case 'Submit':
                return this.submitTravelDisclosure();
            case 'Return':
                return this.returnTravelDisclosure();
            case 'Approve':
                return this.approveTravelDisclosure();
            case 'Withdraw':
                return this.withdrawTravelDisclosure();
            default:
                return;
        }
    }

    leavePageClicked(): void {
        this.service.isAllowNavigation = true;
        this.service.setUnSavedChanges(false, '');
        this.removeSessionStorage();
        this.redirectBasedOnCreateDisclosure();
    }

    removeNewTravelCreateObject(): void {
        this.service.isCreateNewTravelDisclosure = false;
        this.newTravelDisclosureCreateObject = new TravelCreateModalDetails();
    }

    private removeSessionStorage(): void {
        if (!this._navigationService.navigationGuardUrl.includes('create-travel-disclosure')) {
            this._dataStore.removeCreateModalDetails();
        }
    }

    private redirectBasedOnCreateDisclosure(): void {
        if (this.service.isCreateNewTravelDisclosure) {
            this.resetTravelDisclosure();
            this.router.navigate([CREATE_TRAVEL_DISCLOSURE_ROUTE_URL], { queryParams: { disclosureId: null }, queryParamsHandling: 'merge'});
        } else {
            this.router.navigateByUrl(this._navigationService.navigationGuardUrl); // navigateByUrl is used due to "navigationGuardUrl" may contain querry params;
        }
    }

    private setCreateModalDetails(): void {
        if (this.service.isCreateNewTravelDisclosure) {
            this._dataStore.setCreateModalDetails(this.newTravelDisclosureCreateObject);
            this.newTravelDisclosureCreateObject = new TravelCreateModalDetails();
        }
    }

    saveTravelDisclosure(): void {
        this.service.saveSubject.next('SAVE_DISCLOSURE');
    }

    submitTravelDisclosure(): void {
        const reqObject: CoiTravelDisclosure = this._dataStore.getTravelDisclosureRO();
        this.$subscriptions.push(this.service.submitTravelDisclosure(reqObject)
            .subscribe((res: any) => {
                if (res) {
                    this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Travel Disclosure Submitted Successfully');
                    this.service.setUnSavedChanges(false, '');
                    this.router.navigate([POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL],
                        { queryParams: { disclosureId: this.travelDisclosure.travelDisclosureId } });
                }
            }, (err) => {
                if (err.status === 405) {
                    closeCommonModal('travel-confirmation-modal');
                    this.service.concurrentUpdateAction = 'Submit Travel Disclosure';
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in Submitting Travel Disclosure');
                }
            })
        );
    }

    withdrawTravelDisclosure(): void {
        this.$subscriptions.push(this.service.withdrawTravelDisclosure(this.getActionRequestObject())
            .subscribe((res: any) => {
                if (res) {
                    this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Travel Disclosure Withdrawn Successfully.');
                    this.service.setUnSavedChanges(false, '');
                    this.router.navigate([CREATE_TRAVEL_DISCLOSURE_ROUTE_URL],
                        { queryParams: { disclosureId: this.travelDisclosure.travelDisclosureId } });
                }
            }, (err) => {
                if (err.status === 405) {
                    closeCommonModal('travel-confirmation-modal');
                    this.service.concurrentUpdateAction = 'Withdraw Travel Disclosure';
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in Withdrawing Travel Disclosure');
                }
            })
        );
    }

    returnTravelDisclosure(): void {
        this.$subscriptions.push(this.service.returnTravelDisclosure(this.getActionRequestObject())
            .subscribe((res: any) => {
                if (res) {
                    this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Travel Disclosure Returned Successfully');
                    this.service.setUnSavedChanges(false, '');
                    this.reRoutePage();
                }
            }, (err) => {
                if (err.status === 405) {
                    closeCommonModal('travel-confirmation-modal');
                    this.service.concurrentUpdateAction = 'Return Travel Disclosure';
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in Returning Travel Disclosure');
                }
            })
        );
    }

    approveTravelDisclosure(): void {
        this.$subscriptions.push(this.service.approveTravelDisclosure(this.getActionRequestObject())
            .subscribe((res: any) => {
                if (res) {
                    this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Travel Disclosure Approved Successfully');
                    this.service.setUnSavedChanges(false, '');
                    this.setApprovalChangesToDisclosure(res);
                }
            }, (err) => {
                if (err.status === 405) {
                    closeCommonModal('travel-confirmation-modal');
                    this.service.concurrentUpdateAction = 'Approve Travel Disclosure';
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in Approving Travel Disclosure');
                }
            })
        );
    }

    openRiskSlider() {
        this.$subscriptions.push(this.service.riskAlreadyModified({
            'riskCategoryCode': this.travelDisclosure.riskCategoryCode,
            'travelDisclosureId': this.travelDisclosure.travelDisclosureId
        }).subscribe((data: any) => {
            this.isOpenRiskSlider = true;
        }, err => {
            if (err.status === 405) {
                this.service.concurrentUpdateAction = 'Disclosure Risk Status';
            } else {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in modifying risk. Please try again.');
            }
        }))
    }

    closeSlider(event: any): void {
        this.isOpenRiskSlider = false;
    }

    changeDataStoreRisk(event: any) {
        this.travelDisclosure.riskCategoryCode = event.riskCategoryCode;
        this.travelDisclosure.riskLevel = event.riskLevel;
        this._dataStore.setStoreData(this.travelDisclosure);
    }

    cancelConcurrency() {
        this.service.concurrentUpdateAction = '';
    }

    navigateTo(tab: TabType): void {
        const ROUTES: any = {
            TRAVEL_DETAILS: 'coi/create-travel-disclosure/travel-details',
            CERTIFY: 'coi/create-travel-disclosure/certification',
            HISTORY_CREATE: 'coi/create-travel-disclosure/history',
            HISTORY_VIEW: 'coi/travel-disclosure/history',
            SUMMARY: 'coi/travel-disclosure/summary',
            RELATED_DISCLOSURES: 'coi/travel-disclosure/related-disclosures'
        };

        if (ROUTES[tab]) {
            window.scrollTo(0, 0);
            this.router.navigate([ROUTES[tab]], { queryParamsHandling: 'preserve' });
        }
    }

    openPersonSlider(type: string, count: number): void {
        if(count) {
            this.isShowPersonSlider = true;
            this.selectedPersonSliderType = type;
        }
    }

    closePersonSlider(): void {
        this.isShowPersonSlider = false;
        this.selectedPersonSliderType = '';
    }

}

