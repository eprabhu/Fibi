import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, CanActivate, CanDeactivate, Router, RouterStateSnapshot } from '@angular/router';
import { TravelDisclosureService } from './travel-disclosure.service';
import { TravelCreateModalDetails, TravelDisclosureResponseObject } from './travel-disclosure-interface';
import { Observable, Subscriber, forkJoin } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { CommonService } from '../common/services/common.service';
import { HOME_URL, HTTP_ERROR_STATUS } from '../app-constants';

@Injectable()
export class TravelRouteGuardService implements CanActivate, CanDeactivate<boolean> {

    constructor(private _service: TravelDisclosureService,
        private _commonService: CommonService,
        private _router: Router) { }

    canActivate(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot):  boolean | Observable<boolean> {
        this._service.coiTravelDisclosure = new TravelDisclosureResponseObject();
        const MODULE_ID = route.queryParamMap.get('disclosureId');
        if (this.getHomeAndPersonId()) {
            return true;
        } else if (MODULE_ID) {
            return new Observable<boolean>((observer: Subscriber<boolean>) => {
                this.loadTravelDisclosure(MODULE_ID).subscribe((res: TravelDisclosureResponseObject) => {
                    this._service.coiTravelDisclosure = res;
                    observer.next(true);
                    observer.complete();
                });
            });
        } else {
            return false;
        }
    }

    canDeactivate(): boolean {
        if (!this._service.isChildRouting && this.getHomeAndPersonId()) {
            document.getElementById('hidden-validate-button').click();
            return false;
        } else if (this._service.travelDataChanged) {
            document.getElementById('hidden-validate-button').click();
            return false;
        }
        this._service.isChildRouting = false;
        return true;
    }

    private getHomeAndPersonId(): boolean {
        const travelCreateModalDetails: TravelCreateModalDetails = JSON.parse(sessionStorage.getItem('travelCreateModalDetails'));
        const homeUnit = travelCreateModalDetails?.homeUnit || null;
        const personId = travelCreateModalDetails?.personId || null;
        return (homeUnit && personId) ? true : false;
    }

    private loadTravelDisclosure(disclosureId: string): Observable<TravelDisclosureResponseObject> {
        return this._service.loadTravelDisclosure(disclosureId).pipe((catchError(error => this.redirectOnError(error))));
    }

    private redirectOnError(error): Observable<any> {
        this._commonService.showToast(HTTP_ERROR_STATUS, (error.error) ?
            error.error : 'Something went wrong. Please try again.');
        if (error.status === 403 && error.error !== 'DISCLOSURE_EXISTS') {
            this._commonService.forbiddenModule = '8';
            this._router.navigate(['/coi/error-handler/403']);
            return new Observable(null);
        } else {
            this._router.navigate([HOME_URL]);
            return new Observable(null);
        }
    }
}
