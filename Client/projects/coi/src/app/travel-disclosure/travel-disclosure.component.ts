import { Component, OnInit, OnDestroy } from '@angular/core';
import { slideHorizontal } from 'projects/fibi/src/app/common/utilities/animations';
import { ActivatedRoute, Router } from '@angular/router';
import { TravelDisclosureService } from './services/travel-disclosure.service';
import { CommonService } from '../common/services/common.service';
import { Subscription } from 'rxjs';
import { SfiService } from '../disclosure/sfi/sfi.service';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { CoiTravelDisclosure, TravelCreateModalDetails, TravelDisclosureResponseObject } from './travel-disclosure-interface';
import { environment } from '../../environments/environment';
import { TravelDataStoreService } from './services/travel-data-store.service';
import {
    ADMIN_DASHBOARD_URL,
    CREATE_TRAVEL_DISCLOSURE_ROUTE_URL,
    HOME_URL,
    HTTP_ERROR_STATUS,
    HTTP_SUCCESS_STATUS,
    POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL
} from '../app-constants';

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
    responseObject = new TravelDisclosureResponseObject();

    isCreateMode = true;
    isCardExpanded = true;
    ispersondetailsmodal = false;
    isTravelAdministrator = false;

    currentPersonId = '';
    userDetails = { fullName: '', personId: '', homeUnit: null, homeUnitName: null };

    constructor(
        public router: Router,
        public sfiService: SfiService,
        private _route: ActivatedRoute,
        public commonService: CommonService,
        public service: TravelDisclosureService,
        private _dataStore: TravelDataStoreService) {
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
            const travelCreateModalDetails: TravelCreateModalDetails = this._dataStore.getCreateModalDetails();
            const homeUnit = (travelCreateModalDetails && travelCreateModalDetails.homeUnit) || null;
            if (!homeUnit && !MODULE_ID) {
                this.router.navigate([HOME_URL]);
            }
        });
    }

    private clearAllDetails(): void {
        this.responseObject = new TravelDisclosureResponseObject();
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

    private setUserDetails(): void {
        const travelCreateModalDetails: TravelCreateModalDetails = this._dataStore.getCreateModalDetails();
        this.userDetails.personId = travelCreateModalDetails.personId;
        this.userDetails.homeUnit = travelCreateModalDetails.homeUnit;
        this.userDetails.homeUnitName = travelCreateModalDetails.homeUnitName;
        this.userDetails.fullName = this.commonService.getCurrentUserDetail('fullName');
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private setApprovalChangesToDisclosure(resObject): void {
        this.responseObject.reviewStatus = resObject.reviewStatus;
        this.responseObject.reviewStatusCode = resObject.reviewStatusCode;
        this.responseObject.disclosureStatus = resObject.disclosureStatus;
        this.responseObject.disclosureStatusCode = resObject.disclosureStatusCode;
        this.responseObject.documentStatus = resObject.documentStatus;
        this.responseObject.documentStatusCode = resObject.documentStatusCode;
        this.responseObject.acknowledgeAt = resObject.acknowledgeAt;
        this.responseObject.acknowledgeBy = resObject.acknowledgeBy;
        this._dataStore.manualDataUpdate(this.responseObject);
    }

    navigateBack(): void {
        const PREVIOUS_MODULE_URL = this.service.PREVIOUS_MODULE_URL;
        const ROUTE_URL = this.service.isAdminDashboard ? ADMIN_DASHBOARD_URL : HOME_URL;
        (PREVIOUS_MODULE_URL && PREVIOUS_MODULE_URL.includes('travel-disclosure')) ?
            this.router.navigate([ROUTE_URL]) : this.router.navigateByUrl(PREVIOUS_MODULE_URL || ROUTE_URL);
    }

    SaveTravelDisclosure(): void {
        this.service.saveOrCopySubject.next('SAVE_DISCLOSURE');
    }

    isRouteComplete(possibleActiveRoutes: string[] = []): boolean {
        return possibleActiveRoutes.some(paths => this.router.url.includes(paths));
    }

    closePersonDetailsModal(event: any): void {
        this.ispersondetailsmodal = event;
    }

    checkReviewStatusCode(statusCode: string[] | string, method: Method = 'EVERY'): boolean {
        const statusArray = Array.isArray(statusCode) ? statusCode : [statusCode];
        if (method === 'EVERY') {
            return statusArray.every((status) => status.includes(this.responseObject.reviewStatusCode));
        } else {
            return statusArray.some((status) => status.includes(this.responseObject.reviewStatusCode));
        }
    }

    showHomeButton(): boolean {
        return this.isCreateMode || this.checkReviewStatusCode(['7', '2'], 'SOME');
    }

    showReturnOrApproveButton(): boolean {
        return (
            this.isTravelAdministrator &&
            this.checkReviewStatusCode(['3', '2'], 'SOME') &&
            !!this.responseObject.adminPersonId
        );
    }

    closeAssignAdministratorModal(event): void {
        if (event.adminPersonId || event.adminGroupId) {
            this.responseObject.adminPersonId = event.adminPersonId || null;
            this.responseObject.adminPersonName = event.adminPersonName || null;
            this.responseObject.adminGroupId = event.adminGroupId || null;
            this.responseObject.adminGroupName = event.adminGroupName || null;
            this.responseObject.reviewStatusCode = event.reviewStatusCode;
            this.responseObject.reviewStatus = event.reviewStatus;
            this._dataStore.manualDataUpdate(this.responseObject);
        }
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
        this.$subscriptions.push(this.service.withdrawTravelDisclosure(this.responseObject.travelDisclosureId)
            .subscribe((res: any) => {
                if (res) {
                    this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Travel Disclosure Withdrawed Successfully');
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
        this.$subscriptions.push(this.service.returnTravelDisclosure(this.responseObject.travelDisclosureId)
            .subscribe((res: any) => {
                if (res) {
                    this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Travel Disclosure Returned Successfully');
                    this.service.setUnSavedChanges(false, '');
                    this.router.navigate([CREATE_TRAVEL_DISCLOSURE_ROUTE_URL],
                        { queryParams: { disclosureId: this.responseObject.travelDisclosureId } });
                }
            }, (err) => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in Returning Travel Disclosure');
            })
        );
    }

    approveTravelDisclosure(): void {
        this.$subscriptions.push(this.service.approveTravelDisclosure(this.responseObject.travelDisclosureId)
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

