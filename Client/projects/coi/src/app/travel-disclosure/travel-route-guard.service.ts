import { Injectable } from '@angular/core';
import { CanActivate, CanDeactivate, Router } from '@angular/router';
import { TravelDisclosureService } from './travel-disclosure.service';
import { CommonService } from '../common/services/common.service';

@Injectable()
export class TravelRouteGuardService implements CanActivate, CanDeactivate<boolean> {

    constructor(private _service: TravelDisclosureService, private _commonService: CommonService,
                private _router: Router) { }

    canActivate(): boolean {
        const travelDisclosureRO = JSON.parse(sessionStorage.getItem('travelDisclosureRO'));
        const homeUnit = travelDisclosureRO.homeUnit;
        const personId = travelDisclosureRO.personId;
        if (homeUnit && personId) {
            return true;
        }
        this._router.navigate(['/coi/user-dashboard']);
        return false;
    }

    canDeactivate(): boolean {
        if (this._service.travelDataChanged) {
            document.getElementById('hidden-validate-button').click();
            return false;
        } else {
            return true;
        }
    }
}
