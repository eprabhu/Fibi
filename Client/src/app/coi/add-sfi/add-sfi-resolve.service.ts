import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, Router, RouterStateSnapshot } from '@angular/router';
import { forkJoin, Observable, Subscriber } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { CommonService } from '../../common/services/common.service';
import { AddSfiService } from './add-sfi.service';

@Injectable()
export class AddSfiResolveService {

  constructor(
    private _commonService: CommonService,
    private _router: Router,
    public _addSFIService: AddSfiService
  ) { }

  canActivate(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot): Observable<boolean> {

    return new Observable<boolean>((observer: Subscriber<boolean>) => {
      forkJoin(this.getHttpRequests(route)).subscribe((res: any[]) => {
        if (res.length > 1) {
          this.hideManualLoader();
        }
        if (res[0]) {
          this.updateSFIDataStore(res[0], res[1]);
          observer.next(true);
          observer.complete();
        } else {
          observer.next(false);
          observer.complete();
        }
      });
    });
  }

  private updateSFIDataStore(lookups: any, sfiDetails?: any) {
    this._addSFIService.lookups = lookups;
    this._addSFIService.sfiDetails = sfiDetails ? sfiDetails : null;
  }

  private getHttpRequests(route: ActivatedRouteSnapshot): Observable<any>[] {
    const HTTP_REQUESTS = [];
    const SFI_ID = route.queryParamMap.get('entityId');
    HTTP_REQUESTS.push(this.addSFILookUp());
    if (SFI_ID) {
      HTTP_REQUESTS.push(this._addSFIService.getSFIDetails(SFI_ID));
    }
    return HTTP_REQUESTS;
  }


  private addSFILookUp() {
    return this._addSFIService.addSFILookUp().pipe((catchError(error => this.redirectOnError(error))));
  }

  private hideManualLoader() {
    this._commonService.isShowLoader.next(false);
    this._commonService.isManualLoaderOn = false;
  }

  private redirectOnError(error) {
    if (error.status === 403) {
      this._commonService.forbiddenModule = '8';
      this._router.navigate(['/fibi/error/403']);
      return new Observable(null);
    } else {
      this._router.navigate(['/fibi/dashboard/coi-list']);
      return new Observable(null);
    }
  }

}
