import { Injectable } from '@angular/core';
import { DataStoreService } from './data-store.service';
import { NextObserver, Observable, Subscriber, Subscription, forkJoin } from 'rxjs';
import { ApplicableQuestionnaire, COI, getApplicableQuestionnaireData } from '../coi-interface';
import { ActivatedRouteSnapshot } from '@angular/router';
import { CommonService } from '../../common/services/common.service';
import { CoiService, certifyIfQuestionnaireCompleted } from './coi.service';
import { map } from 'rxjs/operators';
import { openCommonModal } from '../../common/utilities/custom-utilities';
import { COMMON_ERROR_TOAST_MSG, HTTP_ERROR_STATUS } from '../../app-constants';

@Injectable()
export class RouterGuardService  {
    $subscriptions: Subscription[] = [];
    ModuleId: string;
    certificationText: string;
     REQUESTREPORTDATA = {
        coiDisclosure: {
            disclosureId: 'ModuleId',
            certificationText: 'certificationText'
        }
    };
    error: string;
    constructor(private _dataStore: DataStoreService, private _commonService: CommonService, private _coiService: CoiService) { }

    canActivate(route: ActivatedRouteSnapshot): Observable<boolean> {
        this._coiService.certificationResponseErrors = [];
        this.ModuleId = route.queryParamMap.get('disclosureId');
        const disclosureId = this.ModuleId ? Number(this.ModuleId) : null;
        return new Observable<boolean>((observer: NextObserver<boolean>) => {
           forkJoin(this._coiService.getApplicableQuestionnaire(this.getApplicationQuestionnaireRO()), this.evaluateValidation())
                .subscribe((res: any) => {
                    if (res) {
                        res[0].applicableQuestionnaire = [];
                        this.checkQuestionnaireCompleted(res[0]);
                        observer.next(true);
                    } else {
                        observer.next(true);
                    }
                    res[1].map((error) => {
                        this._coiService.certificationResponseErrors.push( error);
                    });
                }, (err: any) => {
                    if (err.status === 405) {
                        this._coiService.concurrentUpdateAction = 'Certification';
                    } else {
                        this._commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
                    }
                });
        });
    }

    canDeactivate(): boolean {
        if (this._dataStore.dataChanged) {
            openCommonModal('disclosure-unsaved-changes-modal');
            return false;
        } else {
            return true;
        }
    }
        getApplicationQuestionnaireRO() {
            return {
                'moduleItemCode': 8,
                'moduleSubItemCode': 0,
                'moduleSubItemKey': 0,
                'moduleItemKey': this.ModuleId,
                'actionUserId': this._commonService.getCurrentUserDetail('personID'),
                'actionPersonName': this._commonService.getCurrentUserDetail('fullName'),
                'questionnaireMode': 'ACTIVE_ANSWERED_UNANSWERED'
            };
        }

        checkQuestionnaireCompleted(res) {
            let errorArray = certifyIfQuestionnaireCompleted(res);
            if(errorArray && errorArray.length) {
                errorArray.forEach(ele => this._coiService.certificationResponseErrors.push(ele));
            }
        }

        evaluateValidation(): any {
            const COI_DATA: COI = this._dataStore.getData();
            return this._coiService.evaluateValidation(COI_DATA.coiDisclosure.disclosureId, COI_DATA.coiDisclosure.disclosureNumber);
        }
    }

