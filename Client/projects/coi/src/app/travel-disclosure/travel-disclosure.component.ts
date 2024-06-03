import { Component, OnInit, OnDestroy } from '@angular/core';
import { slideHorizontal } from '../../../../fibi/src/app/common/utilities/animations';
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
    TravelActionAfterSubmitRO, TravelDisclosure, EntityDetails, ModalSize } from './travel-disclosure-interface';
import {
    REPORTER_HOME_URL, HTTP_ERROR_STATUS, ADMIN_DASHBOARD_URL, HTTP_SUCCESS_STATUS,
    CREATE_TRAVEL_DISCLOSURE_ROUTE_URL, POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL, TRAVEL_REVIEW_STATUS
} from '../app-constants';
import { NavigationService } from '../common/services/navigation.service';
import { DefaultAssignAdminDetails, PersonProjectOrEntity } from '../shared-components/shared-interface';
import { closeCommonModal, openCommonModal } from '../common/utilities/custom-utilities';

type Method = 'SOME' | 'EVERY';

@Component({
    selector: 'app-travel-disclosure',
    templateUrl: './travel-disclosure.component.html',
    styleUrls: ['./travel-disclosure.component.scss'],
    animations: [slideHorizontal]
})

export class TravelDisclosureComponent implements OnInit, OnDestroy {

    deployMap = environment.deployUrl;
    $subscriptions: Subscription[] = [];
    defaultAdminDetails = new DefaultAssignAdminDetails();
    travelDisclosure: TravelDisclosure = new TravelDisclosure();
    entityDetails: EntityDetails = new EntityDetails();
    personEntityDetails: PersonProjectOrEntity = new PersonProjectOrEntity();

    isCreateMode = true;
    isMandatory = false;
    isOpenSlider = false;
    isCardExpanded = true;
    isAddAssignModalOpen = false;
    needDescriptionField = false;
    isTravelAdministrator = false;
    isPersonDetailsModalOpen = false;

    modalSize: ModalSize;
    currentPersonId = '';
    modalHeaderTitle = '';
    modalActionBtnName = '';
    descriptionErrorMsg = '';
    textAreaLabelName = '';
    withdrawErrorMsg = 'Please provide the reason for withdrawing the disclosure';
    returnErrorMsg = 'Please provide the reason for returning the disclosure';
    confirmationModalDescription = '';
    confirmationModalHelpText = '';
    reasonHelpText = '';
    userDetails = {
        fullName: '',
        personId: '',
        homeUnit: null,
        homeUnitName: null
    };
    isOpenRiskSlider = false;
    TRAVEL_REVIEW_STATUS = TRAVEL_REVIEW_STATUS;

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
        this.navigateToHome();
        this.fetchTravelRights();
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.isCreateMode = this._dataStore.getEditModeForDisclosure();
    }

    ngOnDestroy(): void {
        this.clearAllDetails();
    }

    private fetchTravelRights(): void {
        this.currentPersonId = this.commonService.getCurrentUserDetail('personId');
        this.isTravelAdministrator = this.commonService.getAvailableRight('MANAGE_TRAVEL_DISCLOSURE');
    }

    private navigateToHome(): void {
        this._route.queryParams.subscribe(params => {
            const MODULE_ID = params['disclosureId'];
            const homeUnit = this.getHomeUnit();
            if(!MODULE_ID) {
                this.travelDisclosure = new TravelDisclosure();
                this._dataStore.setStoreData(this.travelDisclosure);
                this.service.setUnSavedChanges(false, '');             
            }
            if (!homeUnit && !MODULE_ID) {
                this.router.navigate([REPORTER_HOME_URL]);
            }
        });
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
        if(!this._navigationService.navigationGuardUrl.includes('create')) {
            this._dataStore.removeCreateModalDetails();
        }
        this.service.setUnSavedChanges(false, '');
        subscriptionHandler(this.$subscriptions);
    }

    private getDataFromStore(): void {
        if (this._dataStore.getData().travelDisclosureId) {
            this.travelDisclosure = this._dataStore.getData();
            this.userDetails.personId = this.travelDisclosure.personId;
            this.userDetails.fullName = this.travelDisclosure.personFullName;
            this.userDetails.homeUnit = this.travelDisclosure.homeUnitNumber;
            this.userDetails.homeUnitName = this.travelDisclosure.homeUnitName;
            this.entityDetails = this._dataStore.getEntityDetails();
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
        this.userDetails.homeUnitName = travelCreateModalDetails.homeUnitName;
        this.userDetails.personId = travelCreateModalDetails.personId;
        this.userDetails.homeUnit = travelCreateModalDetails.homeUnit;
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

    navigateBack(): void {
        const PREVIOUS_MODULE_URL = this.service.PREVIOUS_MODULE_URL;
        const ROUTE_URL = this.service.isAdminDashboard ? ADMIN_DASHBOARD_URL : REPORTER_HOME_URL;
        (PREVIOUS_MODULE_URL?.includes('travel-disclosure')) ?
            this.router.navigate([ROUTE_URL]) : this.router.navigateByUrl(PREVIOUS_MODULE_URL || ROUTE_URL);
    }

    isRouteComplete(possibleActiveRoutes: string[] = []): boolean {
        return possibleActiveRoutes.some(paths => this.router.url.includes(paths));
    }

    closePersonDetailsModal(event: any): void {
        this.isPersonDetailsModalOpen = event;
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

    showHomeButton(): boolean {
        return (this.isCreateMode || this.checkReviewStatusCode(['7'], 'SOME'))
            && !this.checkReviewStatusCode(['2', '3'], 'SOME');
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
        this.personEntityDetails.unitDetails = `${this.travelDisclosure?.homeUnitNumber} - ${this.travelDisclosure?.homeUnitName}`;
        this.personEntityDetails.homeUnitName = this.travelDisclosure?.homeUnitName;
        this.personEntityDetails.homeUnit = this.travelDisclosure?.homeUnitNumber;
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
        this.service.setUnSavedChanges(false, '');
        this.handleChildRouting();
        this.redirectBasedOnQueryParam();
    }

    private handleChildRouting(): void {
        if (!this.service.isChildRouteTriggered) {
            this._dataStore.removeCreateModalDetails();
        }
    }

    private redirectBasedOnQueryParam(): void {
        this.router.navigateByUrl(this._navigationService.navigationGuardUrl);
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

    closeSlider(event) {
        this.isOpenRiskSlider = false;
    }

    getWarningClass(typeCode) {
        switch (typeCode) {
            case '1':
                return 'invalid';
            case '2':
                return 'medium-risk';
            case '3':
                return 'low-risk';
            default:
                return;
        }
    }

    changeDataStoreRisk(event) {
        this.travelDisclosure.riskCategoryCode = event.riskCategoryCode;
        this.travelDisclosure.riskLevel = event.riskLevel;
        this._dataStore.setStoreData(this.travelDisclosure);
    }

    cancelConcurrency() {
        this.service.concurrentUpdateAction = '';
    }

}

