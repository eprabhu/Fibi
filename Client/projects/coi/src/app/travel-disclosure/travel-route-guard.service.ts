import { Injectable } from '@angular/core';
import { CanActivate, CanDeactivate, Router } from '@angular/router';
import { TravelDisclosureService } from './travel-disclosure.service';
import { TravelCreateModalDetails } from './travel-disclosure-interface';

@Injectable()
export class TravelRouteGuardService implements CanActivate, CanDeactivate<boolean> {

    constructor(private _service: TravelDisclosureService,
                private _router: Router) { }

    canActivate(): boolean {
        const travelCreateModalDetails: TravelCreateModalDetails = JSON.parse(sessionStorage.getItem('travelCreateModalDetails'));
        const homeUnit = travelCreateModalDetails?.homeUnit || null;
        const personId = travelCreateModalDetails?.personId || null;
        if (homeUnit && personId) {
            return true;
        }
        this._router.navigate(['/coi/user-dashboard']);
        return false;
    }

    canDeactivate(): boolean {
        const travelCreateModalDetails: TravelCreateModalDetails = JSON.parse(sessionStorage.getItem('travelCreateModalDetails'));
        const homeUnit = travelCreateModalDetails?.homeUnit || null;
        const personId = travelCreateModalDetails?.personId || null;
        if (!this._service.isChildRouting && homeUnit && personId) {
            document.getElementById('hidden-validate-button').click();
            return false;
        }
        if (this._service.travelDataChanged) {
            document.getElementById('hidden-validate-button').click();
            return false;
        }
        this._service.isChildRouting = false;
        return true;
    }
}
