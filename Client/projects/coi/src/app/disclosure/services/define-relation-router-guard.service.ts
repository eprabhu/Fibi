import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot } from '@angular/router';
import { Observable, of, forkJoin, NextObserver } from 'rxjs';
import { catchError, map } from 'rxjs/operators';
import { DataStoreService } from './data-store.service';
import { CommonService } from '../../common/services/common.service';
import { DefineRelationshipService } from '../define-relationship/services/define-relationship.service';
import { COI, ProjectSfiRelationLoadRO } from '../coi-interface';
import { DefineRelationshipDataStoreService } from '../define-relationship/services/define-relationship-data-store.service';
import { CoiService } from './coi.service';
import { COMMON_ERROR_TOAST_MSG, HTTP_ERROR_STATUS } from '../../app-constants';

@Injectable()
export class DefineRelationsRouterGuard implements CanActivate {

    constructor(
        private _coiService: CoiService,
        private _dataStore: DataStoreService,
        private _commonService: CommonService,
        private _defineRelationshipService: DefineRelationshipService,
        private _defineRelationshipDataStore: DefineRelationshipDataStoreService,
    ) { }

    canActivate(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot): Observable<boolean> {
        // for clearing define relationship datas
        this._defineRelationshipService.clearAllServiceData();
        this._defineRelationshipDataStore.setStoreData([]);
        this._defineRelationshipService.isEditMode = _state.url.includes('create-disclosure/relationship');
        return new Observable<boolean>((observer: NextObserver<boolean>) => {
            forkJoin([
                this.getLookups(),
                this.getProjectRelations()
            ]).subscribe((res: any) => {
                observer.next(true);
                this._defineRelationshipService.coiStatusList = res[0].coiProjConflictStatusTypes; // for lookup api integration
                this._defineRelationshipDataStore.setStoreData(res[1] ? res[1] : []); // for relationship api integration
                this._defineRelationshipService.configureScrollSpy();
            }, (err: any) => {
                if (err.status === 405) {
                    this._coiService.concurrentUpdateAction = 'Relationships';
                } else {
                    this._commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
                }
            });
        });
    }

    private getLookups(): Observable<any> {
        return this._defineRelationshipService.lookups();
    }

    private getProjectRelations(): Observable<any> {
        const COI_DATA: COI = this._dataStore.getData();
        const PROJECT_SFI_RELATION: ProjectSfiRelationLoadRO = {
            personId: COI_DATA.coiDisclosure.person.personId,
            disclosureId: COI_DATA.coiDisclosure.disclosureId,
            disclosureNumber: COI_DATA.coiDisclosure.disclosureNumber,
            dispositionStatusCode: COI_DATA.coiDisclosure.dispositionStatusCode
        };
        return this._defineRelationshipService.getProjectRelations(PROJECT_SFI_RELATION);
    }

}
