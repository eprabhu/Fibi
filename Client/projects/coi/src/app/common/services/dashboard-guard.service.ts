import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, CanActivate, Router, RouterStateSnapshot } from '@angular/router';
import { CommonService } from './common.service';
import { Observable, Subscriber, forkJoin } from 'rxjs';
import { HttpClient } from '@angular/common/http';

@Injectable()
export class DashboardGuardService implements CanActivate {
    constructor(public _commonService: CommonService, private _http: HttpClient) {
    }

    canActivate(next: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean> {
        return new Observable<boolean>((observer: Subscriber<boolean>) => {
            if(!this._commonService.rightsArray.length) {
                forkJoin(this.generateHttpRequest()).subscribe((res: any) => {
                    if (res.length) {
                            this._commonService.assignFibiBasedRights(res[0]);
                            this._commonService.assignCOIBasedRights(res[1]);
                    }
                    observer.next(true);
                    return observer.complete();
                });
            } else {
                observer.next(true);
                return observer.complete();
            }
        });
    }

    generateHttpRequest() {
        const RO = [];
        RO.push(this._http.get(this._commonService.fibiUrl + '/getAllSystemRights'));
        RO.push(this._http.get(this._commonService.fibiUrl + '/fetchAllCoiRights'))
        return RO;
    }
 }



