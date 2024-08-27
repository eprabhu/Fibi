import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, RouterStateSnapshot, CanDeactivate, Router } from '@angular/router';
import { forkJoin, Observable, Subscriber } from 'rxjs';
import { EntityDataStoreService } from './entity-data-store.service';
import { CommonService } from '../common/services/common.service';
import { InformationAndHelpTextService } from '../common/services/informationAndHelpText.service';
import { EntityManagementService } from './entity-management.service';
import { catchError } from 'rxjs/operators';
import { AutoSaveService } from '../common/services/auto-save.service';
import { NavigationService } from '../common/services/navigation.service';

@Injectable()
export class EntityManagementResolveService {

  private readonly _moduleCode = 'GE26';

  constructor(private _dataStore: EntityDataStoreService, private _commonService: CommonService,
    private _informationAndHelpTextService : InformationAndHelpTextService, private _entityManagementService: EntityManagementService) { }

  canActivate(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot): Observable<boolean> {
    return new Observable<boolean>((observer: Subscriber<boolean>) => {
        forkJoin(this.getHttpRequests(route)).subscribe((res: any[]) => {
            if (res.length > 1) {
                this.updateSectionConfig(res[1]);
            }
            if (res[0]) {
                this.updateEntityDataStore(res[0]);
                observer.next(true);
                observer.complete();
            } else {
                observer.next(true);
                observer.complete();
            }
        });
    });

}

private getHttpRequests(route: ActivatedRouteSnapshot): Observable<any>[] {
    const HTTP_REQUESTS = [];
    const ENTITY_ID = route.queryParamMap.get('entityManageId');
    if (ENTITY_ID) { HTTP_REQUESTS.push(this.loadEntityData(ENTITY_ID)); }
    if (!this.isSectionConfigAlreadyFetched()) {
        HTTP_REQUESTS.push(this.getDisclosureSectionConfig());
    } else {
        this.setModuleConfiguration();
    }
    return HTTP_REQUESTS;
}

updateSectionConfig(sectionData: any): void {
    this._dataStore.entitySectionConfig = this._commonService.getSectionCodeAsKeys(sectionData);
    this.setModuleConfiguration();
}

private async updateEntityDataStore(data: any) {
    this._dataStore.setStoreData(data);
}

private loadEntityData(entityId: string) {
    return this._entityManagementService.getEntityDetails(entityId).pipe((catchError(error => this.redirectOnError(error))));
}

isSectionConfigAlreadyFetched() {
    return Object.keys(this._dataStore.entitySectionConfig).length;
}

getDisclosureSectionConfig() {
    return this._commonService.getDashboardActiveModules(this._moduleCode)
}

setModuleConfiguration() {
    this._informationAndHelpTextService.moduleConfiguration = this._dataStore.entitySectionConfig;
}


private redirectOnError(error) {
    return new Observable(null);
}
}

@Injectable()
export class EntityPathResolveService{

  constructor(private _entityManagementService: EntityManagementService, private _commonService: CommonService) { }
    canDeactivate(component: any, currentRoute: ActivatedRouteSnapshot, currentState: RouterStateSnapshot, nextState: RouterStateSnapshot): any {
        if(this._commonService.hasChangesAvailable) {
            this._commonService.isNavigationStopped = true;
            this._commonService.attemptedPath = nextState.url;
            return false;
        } else {
            this._commonService.isNavigationStopped = false;
            this._commonService.attemptedPath = '';
            return true;
        }
    }

}

@Injectable()
export class EntityConfigurationResolveGuardService {

    private readonly _moduleCode = 'GE26';
    constructor( private _commonService: CommonService ) { }

    resolve() {
        return this._commonService.getDashboardActiveModules(this._moduleCode);
    }

}

