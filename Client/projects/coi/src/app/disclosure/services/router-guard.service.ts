import { Injectable } from '@angular/core';
import { DataStoreService } from './data-store.service';
import { NextObserver, Observable, Subscriber, Subscription, forkJoin } from 'rxjs';
import { ApplicableQuestionnaire, getApplicableQuestionnaireData } from '../coi-interface';
import { ActivatedRouteSnapshot } from '@angular/router';
import { CommonService } from '../../common/services/common.service';
import { CoiService } from './coi.service';
import { map } from 'rxjs/operators';
import { openCommonModal } from '../../common/utilities/custom-utilities';

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
           forkJoin(this._coiService.getApplicableQuestionnaire(this.getApplicationQuestionnaireRO()),
            this._coiService.givecoiID(disclosureId))
                .subscribe((res: any) => {
                    if (res) {
                        this.certifyIfQuestionnaireCompleted(res[0]);
                        observer.next(true);
                    } else {
                        observer.next(true);
                    }
                    res[1].map((error) => {
                        this._coiService.certificationResponseErrors.push( error);
                    });
                });
            });
        }
    canDeactivate(): boolean {
        if (this._dataStore.dataChanged) {
            openCommonModal('disclsoure-unsaved-changes-modal');
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
                'actionUserId': this._commonService.getCurrentUserDetail('personId'),
                'actionPersonName': this._commonService.getCurrentUserDetail('fullName'),
                'questionnaireMode': 'ACTIVE_ANSWERED_UNANSWERED'
            };
        }
        certifyIfQuestionnaireCompleted(res: getApplicableQuestionnaireData) {
            if (res && res.applicableQuestionnaire && res.applicableQuestionnaire.length) {
                if (this.isAllQuestionnaireCompleted(res.applicableQuestionnaire)) {
                } else {
                    let questionnaire_error = {validationMessage: ''};
                    questionnaire_error.validationMessage = 'Please complete the mandatory Questionnaire(s) in the “Screening Questionnaire” section.';
                    this._coiService.certificationResponseErrors.push(questionnaire_error);
                    return false;
                }
            }
        }
        isAllQuestionnaireCompleted(questionnaires: ApplicableQuestionnaire[]) {
            return questionnaires.every(questionnaire => questionnaire.QUESTIONNAIRE_COMPLETED_FLAG === 'Y');
        }
    }

