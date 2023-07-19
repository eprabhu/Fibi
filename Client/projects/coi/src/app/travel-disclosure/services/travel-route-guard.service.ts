import { Injectable } from '@angular/core';
import { catchError } from 'rxjs/operators';
import { Observable, Subscriber } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { TravelDataStoreService } from './travel-data-store.service';
import { TravelDisclosureService } from './travel-disclosure.service';
import { NavigationService } from '../../common/services/navigation.service';
import { TravelCreateModalDetails, TravelDisclosure } from '../travel-disclosure-interface';
import { ActivatedRouteSnapshot, CanActivate, CanDeactivate, Router, RouterStateSnapshot} from '@angular/router';
import { CREATE_TRAVEL_DISCLOSURE_ROUTE_URL, HOME_URL, HTTP_ERROR_STATUS, POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL } from '../../app-constants';


@Injectable()
export class TravelRouteGuardService implements CanActivate, CanDeactivate<boolean> {

    constructor(private _service: TravelDisclosureService,
                private _commonService: CommonService,
                private _router: Router,
                private _dataStore: TravelDataStoreService,
                private _navigationService: NavigationService) {
    }

    canActivate(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot): boolean | Observable<boolean> {
        this._dataStore.setStoreData(new TravelDisclosure());
        this._service.isAdminDashboard = this._router.url.includes('admin-dashboard');
        const MODULE_ID = route.queryParamMap.get('disclosureId');
        if (MODULE_ID) {
            return new Observable<boolean>((observer: Subscriber<boolean>) => {
                this.loadTravelDisclosure(MODULE_ID).subscribe((res: TravelDisclosure) => {
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
            document.getElementById('travel-unsaved-changes-modal-trigger-btn').click();
            return false;
        } else if (this._service.travelDataChanged) {
            document.getElementById('travel-unsaved-changes-modal-trigger-btn').click();

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

    private setPreviousModuleUrl(): void {
        this._service.PREVIOUS_MODULE_URL = this._navigationService.navigationGuardUrl;
    }

    private loadTravelDisclosure(disclosureId): Observable<TravelDisclosure> {
        return this._service.loadTravelDisclosure(disclosureId).pipe((catchError(error => this.redirectOnError(error))));
    }

    private updateTravelDataStore(data: TravelDisclosure): void {
        this._dataStore.setStoreData(data);
    }

    private reRouteIfWrongPath(currentPath: string, reviewStatusCode: string, personId: string, route): void {
        const hasCreateTravelPath = currentPath.includes('create-travel-disclosure');
        const hasCreateUserRight = this._service.checkCreateUserRight(personId);
        const isEditPage = ['1', '4', '5'].includes(reviewStatusCode);

        let reRoutePath = null;

        if (isEditPage) {
            reRoutePath = this.getPathForEditPage(hasCreateUserRight, hasCreateTravelPath);
        } else {
            reRoutePath = this.getPathForViewPage(hasCreateUserRight, hasCreateTravelPath);
        }

        if (reRoutePath) {
            this._router.navigate([reRoutePath], { queryParams: { disclosureId: route.queryParamMap.get('disclosureId') } });
        }
    }

    private getPathForEditPage(hasCreateUserRight: boolean, hasCreateTravelPath: boolean): string | null {
        if (!hasCreateUserRight) {
            this.redirectToErrorPage();
        } else if (!hasCreateTravelPath) {
            return CREATE_TRAVEL_DISCLOSURE_ROUTE_URL;
        }
        return null;
    }

    private getPathForViewPage(hasCreateUserRight: boolean, hasCreateTravelPath: boolean): string | null {
        const hasAdminViewRight = this._commonService.getAvailableRight(['MANAGE_TRAVEL_DISCLOSURE', 'VIEW_TRAVEL_DISCLOSURE']);
        if (!(hasAdminViewRight || hasCreateUserRight)) {
            this.redirectToErrorPage();
        } else if (hasCreateTravelPath) {
            return POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL;
        }
        return null;
    }

    private redirectOnError(error): Observable<any> {
        this._commonService.showToast(HTTP_ERROR_STATUS, (error.error) ?
            error.error : 'Something went wrong. Please try again.');
        if (error.status === 403 && error.error !== 'DISCLOSURE_EXISTS') {
            this._commonService.forbiddenModule = '8';
            this.redirectToErrorPage();
            return new Observable(null);
        } else {
            this._router.navigate([HOME_URL]);
            return new Observable(null);
        }
    }

    private redirectToErrorPage(): void {
        this._router.navigate(['/coi/error-handler/403']);
    }
}
