import { Component, OnInit, OnDestroy } from '@angular/core';
import { slideHorizontal } from 'projects/fibi/src/app/common/utilities/animations';
import { Router } from '@angular/router';
import { TravelDisclosureService } from './travel-disclosure.service';
import { CommonService } from '../common/services/common.service';
import { Subscription } from 'rxjs';
import { SfiService } from '../disclosure/sfi/sfi.service';
import { NavigationService } from '../common/services/navigation.service';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { TravelCreateModalDetails, TravelDisclosureResponseObject } from './travel-disclosure-interface';
import { environment } from '../../environments/environment';
@Component({
    selector: 'app-travel-disclosure',
    templateUrl: './travel-disclosure.component.html',
    styleUrls: ['./travel-disclosure.component.scss'],
    animations: [slideHorizontal]
})
export class TravelDisclosureComponent implements OnInit, OnDestroy {
    isCardExpanded = true;
    $subscriptions: Subscription[] = [];
    travelCreateModalDetails: TravelCreateModalDetails;
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
                public sfiService: SfiService,
                public service: TravelDisclosureService,
                private _navigationService: NavigationService) {
    }

    ngOnInit(): void {
        this.loadDisclosureDetails();
    }

    ngOnDestroy(): void {
        this.clearAllDetails();
        subscriptionHandler(this.$subscriptions);
    }

    private clearAllDetails(): void {
        sessionStorage.removeItem('travelCreateModalDetails');
        this.service.coiTravelDisclosure = new TravelDisclosureResponseObject();
        this.responseObject = new TravelDisclosureResponseObject();
    }

    private loadDisclosureDetails(): void {
        if (this.service.coiTravelDisclosure.travelDisclosureId) {
            this.responseObject = this.service.coiTravelDisclosure;
            this.setUserDetails();
        } else {
            this.travelCreateModalDetails = JSON.parse(sessionStorage.getItem('travelCreateModalDetails'));
            this.userDetails.fullName = this.commonService.getCurrentUserDetail('fullName');
            this.userDetails.personId = this.travelCreateModalDetails.personId;
            this.userDetails.homeUnit = this.travelCreateModalDetails.homeUnit;
            this.userDetails.homeUnitName = this.travelCreateModalDetails.homeUnitName;
        }
    }

    private setUserDetails(): void {
        this.userDetails.homeUnit = this.responseObject.homeUnitNumber;
        this.userDetails.homeUnitName = this.responseObject.homeUnitName;
        this.userDetails.fullName = this.responseObject.personFullName;
        this.userDetails.personId = this.responseObject.personId;
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
        this.service.travelDataChanged = false;
        this.service.unSavedTabName = '';
        this.handleChildRouting();
        this.redirectBasedOnQueryParam();
    }

    private handleChildRouting(): void {
        if (!this.service.isChildRouting) {
            sessionStorage.removeItem('travelCreateModalDetails');
        }
    }

    private redirectBasedOnQueryParam(): void {
        this.router.navigateByUrl(this._navigationService.navigationGuardUrl);
    }

}

