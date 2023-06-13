import { Component, OnInit, OnDestroy } from '@angular/core';
import { slideHorizontal } from 'projects/fibi/src/app/common/utilities/animations';
import { ActivatedRoute, Router } from '@angular/router';
import { TravelDisclosureService } from './services/travel-disclosure.service';
import { CommonService } from '../common/services/common.service';
import { Subscription } from 'rxjs';
import { SfiService } from '../disclosure/sfi/sfi.service';
import { NavigationService } from '../common/services/navigation.service';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { TravelCreateModalDetails, TravelDisclosureResponseObject } from './travel-disclosure-interface';
import { environment } from '../../environments/environment';
import { TravelDataStoreService } from './services/travel-data-store.service';
import { HOME_URL } from '../app-constants';
@Component({
    selector: 'app-travel-disclosure',
    templateUrl: './travel-disclosure.component.html',
    styleUrls: ['./travel-disclosure.component.scss'],
    animations: [slideHorizontal]
})
export class TravelDisclosureComponent implements OnInit, OnDestroy {
    isCardExpanded = true;
    $subscriptions: Subscription[] = [];
    responseObject = new TravelDisclosureResponseObject();
    isSaved = false;
    ispersondetailsmodal = false;
    userDetails = {
        fullName: '',
        personId: '',
        homeUnit: null,
        homeUnitName: null
    };
    deployMap = environment.deployUrl;

    constructor(public commonService: CommonService,
                public router: Router,
                private _route: ActivatedRoute,
                public sfiService: SfiService,
                public service: TravelDisclosureService,
                private _dataStore: TravelDataStoreService,
                private _navigationService: NavigationService) {
    }

    ngOnInit(): void {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.navigateToHome();
    }

    ngOnDestroy(): void {
        this.clearAllDetails();
        subscriptionHandler(this.$subscriptions);
    }
    private navigateToHome() {
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
    }

    private getDataFromStore(): void {
        if (this._dataStore.getData().travelDisclosureId) {
            this.responseObject = this._dataStore.getData();
            this.userDetails.homeUnit = this.responseObject.homeUnitNumber;
            this.userDetails.homeUnitName = this.responseObject.homeUnitName;
            this.userDetails.fullName = this.responseObject.personFullName;
            this.userDetails.personId = this.responseObject.personId;
        } else {
            this.setUserDetails();
        }
    }

    private setUserDetails(): void {
        const travelCreateModalDetails: TravelCreateModalDetails = this._dataStore.getCreateModalDetails();
        this.userDetails.fullName = this.commonService.getCurrentUserDetail('fullName');
        this.userDetails.personId = travelCreateModalDetails.personId;
        this.userDetails.homeUnit = travelCreateModalDetails.homeUnit;
        this.userDetails.homeUnitName = travelCreateModalDetails.homeUnitName;
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    handleTravelDisclosureSubmission(purpose: string): void {
        this.service.saveOrCopySubject.next(purpose);
    }

    isRouteComplete(possibleActiveRoutes: string[] = []): boolean {
        return possibleActiveRoutes.some(paths => this.router.url.includes(paths));
    }

    closePersonDetailsModal(event: any): void {
        this.ispersondetailsmodal = event;
    }

    leavePageClicked(): void {
        this.service.setUnSavedChanges(false, '');
        this.handleChildRouting();
        this.redirectBasedOnQueryParam();
    }

    private handleChildRouting(): void {
        if (!this.service.isChildRouting) {
            this._dataStore.removeCreateModalDetails();
        }
    }

    private redirectBasedOnQueryParam(): void {
        this.router.navigateByUrl(this._navigationService.navigationGuardUrl);
    }

}

