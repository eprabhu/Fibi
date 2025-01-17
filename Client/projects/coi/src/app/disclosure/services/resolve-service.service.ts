import {Injectable} from '@angular/core';
import {ActivatedRouteSnapshot, Router, RouterStateSnapshot} from '@angular/router';
import {forkJoin, Observable, Subscriber, Subscription} from 'rxjs';
import {catchError} from 'rxjs/operators';
import {CommonService} from '../../common/services/common.service';
import {CoiService} from './coi.service';
import {DataStoreService} from './data-store.service';
import {
    CREATE_DISCLOSURE_ROUTE_URL,
    HTTP_ERROR_STATUS,
    REPORTER_HOME_URL,
    POST_CREATE_DISCLOSURE_ROUTE_URL
} from '../../app-constants';
import {NavigationService} from '../../common/services/navigation.service';
import { InformationAndHelpTextService } from '../../common/services/informationAndHelpText.service';

@Injectable()
export class ResolveServiceService {

    private readonly _moduleCode = 'COI8';

    $subscriptions: Subscription[] = [];
    constructor(
        private _commonService: CommonService,
        private _dataStore: DataStoreService,
        private _coiService: CoiService,
        private _router: Router,
        private _navigationService: NavigationService,
        private _informationAndHelpTextService : InformationAndHelpTextService
    ) {
    }

    canActivate(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot): Observable<boolean> {
        this._coiService.previousHomeUrl = this.setPreviousUrlPath(this._navigationService.navigationGuardUrl);
        return new Observable<boolean>((observer: Subscriber<boolean>) => {
            forkJoin(this.getHttpRequests(route)).subscribe((res: any[]) => {
                if (res.length > 1) {
                    this.updateSectionConfig(res[1]);
                }
                if (res[0]) {
                    this.updateProposalDataStore(res[0]);
                    this.rerouteIfWrongPath(_state.url, res[0].coiDisclosure, route);
                    if (['3', '4', '7', '8'].includes(res[0].coiDisclosure.coiReviewStatusType.reviewStatusCode)) {
                        this.getCoiReview(res[0].coiDisclosure, observer);
                    } else {
                        observer.next(true);
                        observer.complete();
                    }
                } else {
                    observer.next(false);
                    observer.complete();
                }
            });
        });

    }

    // 2 - void => dispositionStatusCode
    // 1 - pending, 5 - withdrawn, 6 - returned => reviewStatusCode
    rerouteIfWrongPath(currentPath: string, coiDisclosure: any, route) {
        const { reviewStatusCode, personId, dispositionStatusCode } = coiDisclosure;
        const IS_DISCLOSURE_VOID = dispositionStatusCode === '2';
        const IS_CREATE_URL = currentPath.includes('create-disclosure');
        const IS_READY_FOR_REVIEW = !['1', '5', '6'].includes(reviewStatusCode);
        const IS_CREATE_PERSON = personId === this._commonService.currentUserDetails.personID;
        let reRoutePath;
        if (!IS_CREATE_URL && !IS_READY_FOR_REVIEW && IS_CREATE_PERSON && !IS_DISCLOSURE_VOID) {
            reRoutePath = CREATE_DISCLOSURE_ROUTE_URL;
        } else if (IS_CREATE_URL && (IS_READY_FOR_REVIEW || IS_DISCLOSURE_VOID || (!IS_READY_FOR_REVIEW && !IS_CREATE_PERSON))) {
            reRoutePath = POST_CREATE_DISCLOSURE_ROUTE_URL;
        }
        if (reRoutePath) {
            this._router.navigate([reRoutePath], { queryParams: { disclosureId: route.queryParamMap.get('disclosureId') } });
        }
    }

    setPreviousUrlPath(previousUrl: string) {
        return previousUrl.includes('?') ? REPORTER_HOME_URL : previousUrl;
    }

    private updateProposalDataStore(data: any) {
        this._dataStore.setStoreData(data);
    }

    private getHttpRequests(route: ActivatedRouteSnapshot): Observable<any>[] {
        const HTTP_REQUESTS = [];
        const MODULE_ID = route.queryParamMap.get('disclosureId');
        if (MODULE_ID) { HTTP_REQUESTS.push(this.loadDisclosure(MODULE_ID)); }
        if (!this.isSectionConfigAlreadyFetched()) {
            HTTP_REQUESTS.push(this.getDisclosureSectionConfig());
        } else {
            this.setModuleConfiguration();
        }
        return HTTP_REQUESTS;
    }

    private loadDisclosure(disclosureId: string) {
        return this._coiService.loadDisclosure(disclosureId).pipe((catchError(error => this.redirectOnError(error))));
    }

    private redirectOnError(error) {
            this._commonService.showToast(HTTP_ERROR_STATUS, (error.error) ?
                error.error : 'Something went wrong. Please try again.');
        if (error.status === 403 && error.error !== 'DISCLOSURE_EXISTS') {
            this._commonService.forbiddenModule = '8';
            this._router.navigate(['/coi/error-handler/403']);
            return new Observable(null);
        } else {
            this._router.navigate([REPORTER_HOME_URL]);
            // this._commonService.showToast(HTTP_ERROR_STATUS,
            //     error.error !== 'DISCLOSURE_EXISTS' ? 'Please try again later.' : 'Disclosure already exists.');
            return new Observable(null);
        }
    }

    getCoiReview(coiDisclosure: any, observer: any) {
        this.$subscriptions.push(
            this._coiService.getCoiReview(coiDisclosure.disclosureId, coiDisclosure.dispositionStatusCode).subscribe((res: any) => {
                this._dataStore.updateStore(['coiReviewerList'], { coiReviewerList: res });
                this._coiService.isReviewActionCompleted = this._coiService.isAllReviewsCompleted(res);
                const reviewerDetail = this.getLoggedInReviewerInfo(res);
                if (reviewerDetail) {
                    this._coiService.isStartReview = reviewerDetail.reviewStatusTypeCode === '1' ? true : false;
                    this._coiService.isCompleteReview = reviewerDetail.reviewStatusTypeCode === '3' ? true : false;
                    this._coiService.isDisclosureReviewer = true;
                    this._coiService.$SelectedReviewerDetails.next(reviewerDetail);
                    this._coiService.currentReviewForAction = reviewerDetail;
                }
                observer.next(true);
                observer.complete();
            }, _err => {
                observer.next(false);
                observer.complete();
                this._router.navigate([this._coiService.previousHomeUrl]);
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
            }));
    }

    getLoggedInReviewerInfo(coiReviewerList): any {
        const getReviewerDetail = coiReviewerList.find(item => item.assigneePersonId ===
            this._commonService.currentUserDetails.personID && item.reviewStatusTypeCode != '2');
        return getReviewerDetail;
    }

    isSectionConfigAlreadyFetched() {
        return Object.keys(this._dataStore.disclosureSectionConfig).length;
    }

    getDisclosureSectionConfig() {
        return this._commonService.getDashboardActiveModules(this._moduleCode)
    }

    updateSectionConfig(sectionData: any): void {
        this._dataStore.disclosureSectionConfig = this._commonService.getSectionCodeAsKeys(sectionData);
        this.setModuleConfiguration();
    }

    setModuleConfiguration() {
        this._informationAndHelpTextService.moduleConfiguration = this._dataStore.disclosureSectionConfig;
    }

}
