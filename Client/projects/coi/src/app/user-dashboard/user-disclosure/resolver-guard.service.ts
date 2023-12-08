import {Injectable} from '@angular/core';
import {ActivatedRouteSnapshot, CanActivate, Router, RouterStateSnapshot} from '@angular/router';
import {CommonService} from '../../common/services/common.service';
import {Observable, Subscriber} from 'rxjs';
import {HttpClient} from '@angular/common/http';
import {HeaderService} from '../../common/header/header.service';

@Injectable()
export class ResolverGuardService implements CanActivate {

    constructor(private _router: Router,
                private _commonService: CommonService,
                private _http: HttpClient,
                private _headerService: HeaderService) {
    }

    canActivate(next: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean> {
        return new Observable((observer: Subscriber<boolean>) => {
            this.getActiveDisclosure().subscribe((res: any) => {
                this._headerService.activeDisclosures = res.coiDisclosures || [];
                this._headerService.activeOPAs = res.opaDisclosure || [];
                observer.next(true);
            }, err => observer.next(false));
        });
    }

    getActiveDisclosure() {
        return this._http.get(this._commonService.baseUrl + '/getActiveDisclosures');
    }
}
