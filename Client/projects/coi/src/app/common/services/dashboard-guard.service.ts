import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, CanActivate, Router, RouterStateSnapshot } from '@angular/router';
import { CommonService } from './common.service';
import { Observable, Subscriber } from 'rxjs';

@Injectable()
export class DashboardGuardService implements CanActivate {
    constructor(private _router: Router, public _commonService: CommonService) {
    }

    canActivate(next: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean> {
        return new Observable((observer: Subscriber<boolean>) => {
            // if(this._commonService.isValidUser) {
            //     observer.next(true);
            // } else {
            //     this._router.navigate(['/login']);
            //     observer.next(false);
            // }
            observer.next(true);
        });
    }
}



