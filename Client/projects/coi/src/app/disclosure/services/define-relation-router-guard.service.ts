import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot } from '@angular/router';
import { Observable, of, forkJoin, NextObserver } from 'rxjs';
import { catchError, map } from 'rxjs/operators';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { DataStoreService } from './data-store.service';
import { CommonService } from '../../common/services/common.service';
import { DefineRelationshipService } from '../define-relationship/services/define-relationship.service';
import { COI } from '../coi-interface';
import { DefineRelationshipDataStoreService } from '../define-relationship/services/define-relationship-data-store.service';

@Injectable()
export class DefineRelationsRouterGuard implements CanActivate {

    constructor(
        private _dataStore: DataStoreService,
        private _commonService: CommonService,
        private _defineRelationshipService: DefineRelationshipService,
        private _defineRelationshipDataStore: DefineRelationshipDataStoreService,
    ) { }

    canActivate(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot): Observable<boolean> {
        this._defineRelationshipService.isEditMode = _state.url.includes('create-disclosure/relationship');
        this._defineRelationshipService.coiStatusList = []; // Clear any previous status list
        return new Observable<boolean>((observer: NextObserver<boolean>) => {
            forkJoin([
                this.getLookups(),
                this.getProjectRelations()
            ]).subscribe((res: any) => {
                observer.next(true);
            });
        });
    }

    private getLookups(): Observable<any> {
        return this._defineRelationshipService.lookups().pipe(
            map((res: any) => {
                this._defineRelationshipService.coiStatusList = res.coiProjConflictStatusTypes;
                return res; // Return the response for further processing in subscribe
            }),
            catchError((_error: any) => {
                return of(null); // Return null on error
            })
        );
    }

    private getProjectRelations(): Observable<any> {
        const COI_DATA: COI = this._dataStore.getData();
        const PROJECT_SFI_RELATION = {
            disclosureId: COI_DATA.coiDisclosure.disclosureId,
            disclosureNumber: COI_DATA.coiDisclosure.disclosureNumber,
            personId: COI_DATA.coiDisclosure.person.personId
        };

        return this._defineRelationshipService.getProjectRelations(PROJECT_SFI_RELATION).pipe(
            map((res: any) => {
                if (res) {
                    this._defineRelationshipDataStore.setStoreData(res);
                }
                this._defineRelationshipService.configureScrollSpy();
                return res; // Return the response for further processing in subscribe
            }),
            catchError((_error: any) => {
                return of(null); // Return null on error
            })
        );
    }

}
