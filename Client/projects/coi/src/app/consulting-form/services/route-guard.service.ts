import { Injectable } from '@angular/core';
import { Observable, NextObserver } from 'rxjs';
import { openCommonModal } from '../../common/utilities/custom-utilities';
import { ConsultingService } from './consulting-service.service';

@Injectable()
export class RouteGuardService {

  constructor(private _consultingService: ConsultingService) { }

    canActivate(): Observable<boolean> {
        return new Observable<boolean>((observer: NextObserver<boolean>) => {
            observer.next(true);
        });
    }

    canDeactivate(): boolean {
        if (this._consultingService.isFormBuilderDataChangePresent || this._consultingService.isDataChangeAvailableInEntity) {
            openCommonModal('consultingForm-unsaved-changes-modal');
            return false;
        } else {
            return true;
        }
    }
}
