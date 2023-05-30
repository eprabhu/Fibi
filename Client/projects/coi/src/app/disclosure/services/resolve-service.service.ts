import {Injectable, OnDestroy} from '@angular/core';
import {ActivatedRouteSnapshot, Router, RouterStateSnapshot} from '@angular/router';
import {forkJoin, Observable, Subscriber} from 'rxjs';
import {catchError} from 'rxjs/operators';

import {CommonService} from '../../common/services/common.service';
import {CoiService} from './coi.service';
import {DataStoreService} from './data-store.service';
import {
    CREATE_DISCLOSURE_ROUTE_URL,
    HTTP_ERROR_STATUS,
    HOME_URL,
    POST_CREATE_DISCLOSURE_ROUTE_URL
} from '../../app-constants';
import {NavigationService} from '../../common/services/navigation.service';

@Injectable()
export class ResolveServiceService {

    constructor(
        private _commonService: CommonService,
        private _dataStore: DataStoreService,
        private _coiService: CoiService,
        private _router: Router,
        private _navigationService: NavigationService
    ) {
    }

    canActivate(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot): Observable<boolean> {
        this._coiService.previousHomeUrl = this.setPreviousUrlPath(this._navigationService.navigationGuardUrl);
        return new Observable<boolean>((observer: Subscriber<boolean>) => {
            forkJoin(this.getHttpRequests(route)).subscribe((res: any[]) => {
                if (res.length > 1) {
                    this.hideManualLoader();
                }
                if (res[0]) {
                    this.updateProposalDataStore(res[0]);
                    this.rerouteIfWrongPath(_state.url, res[0].coiDisclosure.reviewStatusCode, route);
                    observer.next(true);
                    observer.complete();
                } else {
                    observer.next(false);
                    observer.complete();
                }
            });
        });

    }

    rerouteIfWrongPath(currentPath: string, reviewStatusCode: string, route) {
        let reRoutePath;
        if (reviewStatusCode === '1' && !currentPath.includes('create-disclosure')) {
            reRoutePath = CREATE_DISCLOSURE_ROUTE_URL;
        } else if (reviewStatusCode !== '1' && currentPath.includes('create-disclosure')) {
            reRoutePath = POST_CREATE_DISCLOSURE_ROUTE_URL;
        }
        if (reRoutePath) {
            this._router.navigate([reRoutePath], {queryParams: {disclosureId: route.queryParamMap.get('disclosureId')}});
        }
    }

    setPreviousUrlPath(previousUrl: string) {
        return previousUrl.includes('?') ? HOME_URL : previousUrl;
    }

    private updateProposalDataStore(data: any) {
        this._dataStore.setStoreData(data);
    }

    private getHttpRequests(route: ActivatedRouteSnapshot): Observable<any>[] {
        const HTTP_REQUESTS = [];
        const MODULE_ID = route.queryParamMap.get('disclosureId');
        if (MODULE_ID) { HTTP_REQUESTS.push(this.loadDisclosure(MODULE_ID)); }
        return HTTP_REQUESTS;
    }

    private loadDisclosure(disclosureId: string) {
        return this._coiService.loadDisclosure(disclosureId).pipe((catchError(error => this.redirectOnError(error))));
    }

    private hideManualLoader() {
        this._commonService.isShowLoader.next(false);
        this._commonService.isManualLoaderOn = false;
    }

    private redirectOnError(error) {
            this._commonService.showToast(HTTP_ERROR_STATUS, (error.error) ?
                error.error : 'Something went wrong. Please try again.');
        if (error.status === 403 && error.error !== 'DISCLOSURE_EXISTS') {
            this._commonService.forbiddenModule = '8';
            this._router.navigate(['/coi/error-handler/403']);
            return new Observable(null);
        } else {
            this._router.navigate([HOME_URL]);
            // this._commonService.showToast(HTTP_ERROR_STATUS,
            //     error.error !== 'DISCLOSURE_EXISTS' ? 'Please try again later.' : 'Disclosure already exists.');
            return new Observable(null);
        }
    }

}
