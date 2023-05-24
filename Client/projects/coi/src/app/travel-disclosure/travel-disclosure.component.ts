import { Component, OnInit, OnDestroy, Input } from '@angular/core';
import { slideHorizontal } from 'projects/fibi/src/app/common/utilities/animations';
import { Router } from '@angular/router';
import { TravelDisclosureService } from './travel-disclosure.service';
import { CommonService } from '../common/services/common.service';
import { CoiService } from '../disclosure/services/coi.service';
import { DataStoreService } from '../disclosure/services/data-store.service';
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
    isCreateMode: boolean;
    isSaved = false;
    currentUser: string;
    homeUnit: string;
    ispersondetailsmodal: boolean = false;
    userDetails = {
        fullName: '',
        personId: ''
    };

    constructor(public coiService: CoiService,
        private _commonService: CommonService,
        public router: Router,
        public sfiService: SfiService,
        private _service: TravelDisclosureService,
        private _navigationService: NavigationService,
        private _dataStore: DataStoreService) {
    }

    ngOnInit(): void {
        this.userDetails.fullName = this._commonService.getCurrentUserDetail('fullName');
        this.userDetails.personId = this._commonService.getCurrentUserDetail('personId');
        this.homeUnit = `${this._commonService.getCurrentUserDetail('homeUnit')} - ${this._commonService.getCurrentUserDetail('homeUnitName')}`;
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private getDataFromStore(): void {

    }

    certifyTravelDisclosure(): void {
        this._service.travelDisclosureSubject.next("certify")
    }

    certifyCopyTravelDisclosure(): void {
        this._service.travelDisclosureSubject.next("certifycopy")
    }

    isRouteComplete(possibleActiveRoutes: string[] = []): boolean {
        return possibleActiveRoutes.some(paths => this.router.url.includes(paths));
    }

    closePersonDetailsModal(event: any) {
        this.ispersondetailsmodal = event;
    }

    openPersonDetailModal(): void {
        this.ispersondetailsmodal = true;
    }

    leavePageClicked(): void {
        this._dataStore.dataChanged = false;
        this.coiService.unSavedModules = '';
        this.redirectBasedOnQueryParam();
    }

    private redirectBasedOnQueryParam(): void {
        this.router.navigateByUrl(this._navigationService.navigationGuardUrl);
    }

}

