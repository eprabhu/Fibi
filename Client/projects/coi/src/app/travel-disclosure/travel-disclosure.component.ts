import { Component, OnInit, OnDestroy, Input } from '@angular/core';
import { slideHorizontal } from 'projects/fibi/src/app/common/utilities/animations';
import { Router } from '@angular/router';
import { TravelDisclosureService } from './travel-disclosure.service';
import { CommonService } from '../common/services/common.service';
import { Subscription } from 'rxjs';
import { SfiService } from '../disclosure/sfi/sfi.service';
import { NavigationService } from '../common/services/navigation.service';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
@Component({
    selector: 'app-travel-disclosure',
    templateUrl: './travel-disclosure.component.html',
    styleUrls: ['./travel-disclosure.component.scss'],
    animations: [slideHorizontal]
})
export class TravelDisclosureComponent implements OnInit, OnDestroy {
    isCardExpanded = true;
    $subscriptions: Subscription[] = [];
    isCreateMode = false;
    isSaved = false;
    currentUser = '';
    ispersondetailsmodal = false;
    userDetails = {
        fullName: '',
        personId: ''
    };
    travelDisclosureRO;

    constructor(private _commonService: CommonService,
                public router: Router,
                public sfiService: SfiService,
                public service: TravelDisclosureService,
                private _navigationService: NavigationService) {
    }

    ngOnInit(): void {
        this.travelDisclosureRO = JSON.parse(sessionStorage.getItem('travelDisclosureRO'));
        this.userDetails.fullName = this._commonService.getCurrentUserDetail('fullName');
        this.userDetails.personId = this.travelDisclosureRO.personId;
    }

    ngOnDestroy(): void {
        sessionStorage.removeItem('travelDisclosureRO');
        subscriptionHandler(this.$subscriptions);
    }

    certifyTravelDisclosure(): void {
        this.service.travelDisclosureSubject.next('certify');
    }

    certifyCopyTravelDisclosure(): void {
        this.service.travelDisclosureSubject.next('certifycopy');
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
        this.redirectBasedOnQueryParam();
    }

    private redirectBasedOnQueryParam(): void {
        this.router.navigateByUrl(this._navigationService.navigationGuardUrl);
    }

}

