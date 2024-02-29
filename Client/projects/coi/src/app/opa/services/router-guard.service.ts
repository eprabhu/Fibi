import {Injectable} from '@angular/core';
import {DataStoreService} from './data-store.service';
import {NextObserver, Observable, Subscription} from 'rxjs';
import {ActivatedRouteSnapshot} from '@angular/router';
import {CommonService} from '../../common/services/common.service';
import {openCommonModal} from '../../common/utilities/custom-utilities';
import {OpaService} from './opa.service';

@Injectable()
export class RouterGuardService {
    $subscriptions: Subscription[] = [];

    constructor(private _dataStore: DataStoreService, private _commonService: CommonService, private _opaService: OpaService) {
    }

    canActivate(route: ActivatedRouteSnapshot): Observable<boolean> {
        return new Observable<boolean>((observer: NextObserver<boolean>) => {
            observer.next(true);
        });
    }

    canDeactivate(): boolean {
        if (this._opaService.isFormBuilderDataChangePresent) {
            openCommonModal('opa-unsaved-changes-modal');
            return false;
        } else {
            return true;
        }
    }
}

