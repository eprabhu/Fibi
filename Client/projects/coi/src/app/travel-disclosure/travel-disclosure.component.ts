import { Component, OnInit, OnDestroy } from '@angular/core';
import { slideHorizontal } from 'projects/fibi/src/app/common/utilities/animations';
import { Router } from '@angular/router';
import { TravelDisclosureService } from './travel-disclosure.service';
import { CommonService } from '../common/services/common.service';
import { Subscription } from 'rxjs';
import { SfiService } from '../disclosure/sfi/sfi.service';
import { NavigationService } from '../common/services/navigation.service';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { TravelCreateModalDetails } from './travel-disclosure-interface';
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
    isCreateMode = false;
    isSaved = false;
    currentUser = '';
    ispersondetailsmodal = false;
    userDetails = {
        fullName: '',
        personId: ''
    };
    deployMap = environment.deployUrl;

    constructor(public _commonService: CommonService,
                public router: Router,
                public sfiService: SfiService,
                public service: TravelDisclosureService,
                private _navigationService: NavigationService) {
    }

    ngOnInit(): void {
        this.travelCreateModalDetails = JSON.parse(sessionStorage.getItem('travelCreateModalDetails'));
        this.userDetails.fullName = this._commonService.getCurrentUserDetail('fullName');
        this.userDetails.personId = this.travelCreateModalDetails.personId;
    }

    ngOnDestroy(): void {
        sessionStorage.removeItem('travelCreateModalDetails');
        subscriptionHandler(this.$subscriptions);
    }

    certifyTravelDisclosure(): void {
        this.service.saveOrCopySubject.next('certify');
    }

    certifyCopyTravelDisclosure(): void {
        this.service.saveOrCopySubject.next('certifycopy');
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

