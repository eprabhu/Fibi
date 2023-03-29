import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, Router, RouterStateSnapshot } from '@angular/router';
import { forkJoin, Observable, Subscriber } from 'rxjs';
import { catchError } from 'rxjs/operators';

import { CommonService } from '../../common/services/common.service';
import { CoiService } from './coi.service';
import { DataStoreService } from './data-store.service';

@Injectable()
export class ResolveServiceService {

    constructor(
        private _commonService: CommonService,
        private _dataStore: DataStoreService,
        private _coiService: CoiService,
        private _router: Router
    ) { }

    canActivate(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot): Observable<boolean> {

        return new Observable<boolean>((observer: Subscriber<boolean>) => {
            forkJoin(this.getHttpRequests(route)).subscribe((res: any[]) => {
                if (res.length > 1) {
                    this.hideManualLoader();
                }
                if (res[0]) {
                    this.updateProposalDataStore(res[0]);
                    observer.next(true);
                    observer.complete();
                } else {
                    observer.next(false);
                    observer.complete();
                }
            });
        });

    }

    private updateProposalDataStore(data: any) {
        this._dataStore.setStoreData(data);
    }

    private getHttpRequests(route: ActivatedRouteSnapshot): Observable<any>[] {
        const HTTP_REQUESTS = [];
        const MODULE_ID = route.queryParamMap.get('disclosureId');
        MODULE_ID ? HTTP_REQUESTS.push(this.loadDisclosure(MODULE_ID)) :
            HTTP_REQUESTS.push(this.createDisclosure(route.queryParamMap.get('tabName')));
        return HTTP_REQUESTS;
    }

    private loadDisclosure(disclosureId: string) {
        return this._coiService.loadDisclosure(disclosureId).pipe((catchError(error => this.redirectOnError(error))));
    }

    private createDisclosure(tabName: string) {
        if (!tabName) {
            this._router.navigate(['/coi/user-dashboard']);
            return null;
        }
        return this._coiService.createDisclosure({ disclosureCategoryType: tabName })
            .pipe((catchError(error => this.redirectOnError(error))));
    }

    private hideManualLoader() {
        this._commonService.isShowLoader.next(false);
        this._commonService.isManualLoaderOn = false;
    }

    private redirectOnError(error) {
        if (error.status === 403 && error.error !== 'DISCLOSURE_EXISTS') {
            this._commonService.forbiddenModule = '8';
            this._router.navigate(['/fibi/error/403']);
            return new Observable(null);
        } else {
            this._router.navigate(['/coi/user-dashboard']);
            // this._commonService.showToast(HTTP_ERROR_STATUS,
            //     error.error !== 'DISCLOSURE_EXISTS' ? 'Please try again later.' : 'Disclosure already exists.');
            return new Observable(null);
        }
    }

}
