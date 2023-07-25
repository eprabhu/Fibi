import { Component, OnInit, OnDestroy } from '@angular/core';
import { slideHorizontal } from 'projects/fibi/src/app/common/utilities/animations';
import { ActivatedRoute, Router } from '@angular/router';
import { TravelDisclosureService } from './services/travel-disclosure.service';
import { CommonService } from '../common/services/common.service';
import { Subscription } from 'rxjs';
import { SfiService } from '../disclosure/sfi/sfi.service';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { environment } from '../../environments/environment';
import { TravelDataStoreService } from './services/travel-data-store.service';
import {
    CoiTravelDisclosure, DefaultAdminDetails, TravelCreateModalDetails,
    TravelActionAfterSubmitRO, TravelDisclosure } from './travel-disclosure-interface';
import {
    HOME_URL, HTTP_ERROR_STATUS, ADMIN_DASHBOARD_URL, HTTP_SUCCESS_STATUS,
    CREATE_TRAVEL_DISCLOSURE_ROUTE_URL, POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL } from '../app-constants';
import { NavigationService } from '../common/services/navigation.service';

type Method = 'SOME' | 'EVERY';
type ModalSize = 'sm' | 'lg' | 'xl' | '';

@Component({
    selector: 'app-travel-disclosure',
    templateUrl: './travel-disclosure.component.html',
    styleUrls: ['./travel-disclosure.component.scss'],
    animations: [slideHorizontal]
})

export class TravelDisclosureComponent implements OnInit, OnDestroy {

    deployMap = environment.deployUrl;
    $subscriptions: Subscription[] = [];
    defaultAdminDetails = new DefaultAdminDetails();
    responseObject = new TravelDisclosure();

    isCreateMode = true;
    isMandatory = false;
    isCardExpanded = true;
    isAddAssignModalOpen = false;
    needDescriptionField = false;
    isTravelAdministrator = false;
    isPersonDetailsModalOpen = false;

    modalSize: ModalSize;
    currentPersonId = '';
    modalHeaderTitle = '';
    modalActionBtnName = '';
    descriptionError = '';
    withdrawError = 'Describe the reason for withdrawing the disclosure';
    returnError = 'Describe the reason for returning the disclosure';
    confirmationModalDescription = '';

    userDetails = {
        fullName: '',
        personId: '',
        homeUnit: null,
        homeUnitName: null
    };
    helpText = [];
    withdrawHelpText = [
        `Withdraw any disclosure in 'Submitted' status.`,
        `Describe the reason for withdrawal in the field provided.`,
        `Click on 'Withdraw' button to recall your disclosure for any modification.`
    ];
    returnHelpText = [
        `Return any disclosure in 'Submitted/Review In Progress' status.`,
        `Describe the reason for returning  in the field provided.`,
        `Click on 'Return' button to return the disclosure for any modification.`
    ];

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
            if (!homeUnit && !MODULE_ID) {
                this.router.navigate([HOME_URL]);
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
        this.responseObject = new TravelDisclosure();
        this._dataStore.setStoreData(this.responseObject);
        this._dataStore.removeCreateModalDetails();
        this.service.setUnSavedChanges(false, '');
        subscriptionHandler(this.$subscriptions);
    }

    private getDataFromStore(): void {
        if (this._dataStore.getData().travelDisclosureId) {
            this.responseObject = this._dataStore.getData();
            this.userDetails.personId = this.responseObject.personId;
            this.userDetails.fullName = this.responseObject.personFullName;
            this.userDetails.homeUnit = this.responseObject.homeUnitNumber;
            this.userDetails.homeUnitName = this.responseObject.homeUnitName;
        } else {
            this.setUserDetails();
        }
    }

    private setAssignAdminModalDetails(): void {
        this.defaultAdminDetails.adminGroupId = this.responseObject.adminGroupId;
        this.defaultAdminDetails.adminPersonId = this.responseObject.adminPersonId;
        this.defaultAdminDetails.adminGroupName = this.responseObject.adminGroupName;
        this.defaultAdminDetails.adminPersonName = this.responseObject.adminPersonName;
    }

    private setModalHeaderTitle(modalActionBtnName: string): void {
        const id = this.responseObject.travelDisclosureId || null;
        const creatorName = this.responseObject.personFullName || null;
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

    private setApprovalChangesToDisclosure(responseObject): void {
        this.responseObject.reviewStatus = responseObject.reviewStatus;
        this.responseObject.acknowledgeAt = responseObject.acknowledgeAt;
        this.responseObject.acknowledgeBy = responseObject.acknowledgeBy;
        this.responseObject.documentStatus = responseObject.documentStatus;
        this.responseObject.updateTimestamp = responseObject.updateTimestamp;
        this.responseObject.reviewStatusCode = responseObject.reviewStatusCode;
        this.responseObject.disclosureStatus = responseObject.disclosureStatus;
        this.responseObject.documentStatusCode = responseObject.documentStatusCode;
        this.responseObject.disclosureStatusCode = responseObject.disclosureStatusCode;
        this._dataStore.manualDataUpdate(this.responseObject);
    }

    private getActionRequestObject(): TravelActionAfterSubmitRO {
        return {
            travelDisclosureId: this.responseObject.travelDisclosureId,
            description: this.confirmationModalDescription
        };
    }

    private reRoutePage(): void {
        if (!this.service.checkCreateUserRight(this.responseObject.personId)) {
            this.navigateBack();
        } else {
            this.router.navigate([CREATE_TRAVEL_DISCLOSURE_ROUTE_URL],
                { queryParams: { disclosureId: this.responseObject.travelDisclosureId } });
        }
    }

    navigateBack(): void {
        const PREVIOUS_MODULE_URL = this.service.PREVIOUS_MODULE_URL;
        const ROUTE_URL = this.service.isAdminDashboard ? ADMIN_DASHBOARD_URL : HOME_URL;
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
            return statusArray.every((status) => status.includes(this.responseObject.reviewStatusCode));
        } else {
            return statusArray.some((status) => status.includes(this.responseObject.reviewStatusCode));
        }
    }

    checkAdministratorRight(): boolean {
        return this.service.checkCreateUserRight(this.responseObject.adminPersonId);
    }

    showReturnOrApproveButton(): boolean {
        return this.checkAdministratorRight() && this.checkReviewStatusCode(['3']);
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
            this.responseObject.adminPersonId = event.adminPersonId || null;
            this.responseObject.adminPersonName = event.adminPersonName || null;
            this.responseObject.adminGroupId = event.adminGroupId || null;
            this.responseObject.adminGroupName = event.adminGroupName || null;
            this.responseObject.reviewStatusCode = event.reviewStatusCode;
            this.responseObject.reviewStatus = event.reviewStatus;
            this.responseObject.updateTimestamp = event.updateTimestamp;
            this._dataStore.manualDataUpdate(this.responseObject);
        }
        this.isAddAssignModalOpen = false;
    }

    openConfirmationModal(actionBtnName: string, needDescriptionField: boolean,
            helpText: [] = [], isMandatory: boolean = false, descriptionError: string = ''): void {
        this.modalActionBtnName = actionBtnName;
        this.needDescriptionField = needDescriptionField;
        this.isMandatory = isMandatory;
        this.helpText = helpText;
        this.modalSize = actionBtnName === 'Submit' ? '' : 'lg';
        this.setModalHeaderTitle(actionBtnName);
        this.descriptionError = descriptionError;
        document.getElementById('travel-confirmation-modal-trigger-btn').click();
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

    leavePageClicked(event: boolean): void {
        if (event) {
            this.service.setUnSavedChanges(false, '');
            this.handleChildRouting();
            this.redirectBasedOnQueryParam();
        }
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
                        { queryParams: { disclosureId: this.responseObject.travelDisclosureId } });
                }
            }, (err) => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in Submitting Travel Disclosure');
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
                        { queryParams: { disclosureId: this.responseObject.travelDisclosureId } });
                }
            }, (err) => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in Withdrawing Travel Disclosure');
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
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in Returning Travel Disclosure');
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
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in Approving Travel Disclosure');
            })
        );
    }

}

