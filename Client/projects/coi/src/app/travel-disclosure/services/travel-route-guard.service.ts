import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, CanActivate, CanDeactivate,
         NavigationEnd, Router, RouterStateSnapshot } from '@angular/router';
import { TravelDisclosureService } from './travel-disclosure.service';
import { TravelCreateModalDetails, TravelDisclosureResponseObject } from '../travel-disclosure-interface';
import { Observable, Subscriber } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { CommonService } from '../../common/services/common.service';
import { CREATE_TRAVEL_DISCLOSURE_ROUTE_URL, HOME_URL, HTTP_ERROR_STATUS,
         POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL } from '../../app-constants';
import { TravelDataStoreService } from './travel-data-store.service';
import { NavigationService } from '../../common/services/navigation.service';

@Injectable()
export class TravelRouteGuardService implements CanActivate, CanDeactivate<boolean> {

    constructor(private _service: TravelDisclosureService,
        private _commonService: CommonService,
        private _router: Router,
        private _dataStore: TravelDataStoreService,
        private _navigationService: NavigationService) {
        }

    canActivate(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot):  boolean | Observable<boolean> {
        this._dataStore.setStoreData(new TravelDisclosureResponseObject());
        this._service.isAdminDashboard = this._router.url.includes('admin-dashboard');
        const MODULE_ID = route.queryParamMap.get('disclosureId');
        if (MODULE_ID) {
            return new Observable<boolean>((observer: Subscriber<boolean>) => {
                this.loadTravelDisclosure(MODULE_ID).subscribe((res: TravelDisclosureResponseObject) => {
                    if (res) {
                        this.updateTravelDataStore(res);
                        this.reRouteIfWrongPath(_state.url, res.reviewStatusCode, res.personId, route);
                        this.setPreviousModuleUrl();
                        observer.next(true);
                        observer.complete();
                    } else {
                        observer.next(false);
                        observer.complete();
                    }
                });
            });
        } else if (this.getHomeAndPersonId()) {
            return true;
        } else {
            this._router.navigate([HOME_URL]);
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
        const travelCreateModalDetails: TravelCreateModalDetails = this._dataStore.getCreateModalDetails();
        const homeUnit = (travelCreateModalDetails && travelCreateModalDetails.homeUnit) || null;
        const personId = (travelCreateModalDetails && travelCreateModalDetails.personId) || null;
        return (homeUnit && personId) ? true : false;
    }

    private setPreviousModuleUrl() {
        this._service.PREVIOUS_MODULE_URL = this._navigationService.navigationGuardUrl;
    }

    private loadTravelDisclosure(disclosureId): Observable<TravelDisclosureResponseObject> {
        return this._service.loadTravelDisclosure(disclosureId).pipe((catchError(error => this.redirectOnError(error))));
    }

    private updateTravelDataStore(data: TravelDisclosureResponseObject): void {
        this._dataStore.setStoreData(data);
    }

    private reRouteIfWrongPath(currentPath: string, reviewStatusCode: string, personId: string, route) {
        let reRoutePath;
        const isCreateMode = ['1', '4', '5'].includes(reviewStatusCode);
        if (this._service.checkCreateUserRight(personId) && isCreateMode && !currentPath.includes('create-travel-disclosure')) {
            reRoutePath = CREATE_TRAVEL_DISCLOSURE_ROUTE_URL;
        } else if (!this._service.checkCreateUserRight(personId) && isCreateMode) {
            this._router.navigate([HOME_URL]);
        } else if (!isCreateMode && currentPath.includes('create-travel-disclosure')) {
            reRoutePath = POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL;
        }
        if (reRoutePath) {
            this._router.navigate([reRoutePath], {queryParams: {disclosureId: route.queryParamMap.get('disclosureId')}});
        }
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
