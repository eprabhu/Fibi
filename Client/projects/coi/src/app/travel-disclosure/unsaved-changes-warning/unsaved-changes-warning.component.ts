import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { NavigationService } from '../../common/services/navigation.service';
import { TravelDataStoreService } from '../services/travel-data-store.service';
import { TravelDisclosureService } from '../services/travel-disclosure.service';

@Component({
    selector: 'app-unsaved-changes-warning',
    templateUrl: './unsaved-changes-warning.component.html',
    styleUrls: ['./unsaved-changes-warning.component.scss']
})
export class UnsavedChangesWarningComponent implements OnInit {

    constructor(public service: TravelDisclosureService,
        private _dataStore: TravelDataStoreService,
        private _router: Router,
        private _navigationService: NavigationService) { }

    ngOnInit() {
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
        this._router.navigateByUrl(this._navigationService.navigationGuardUrl);
    }

}
