import {Injectable} from '@angular/core';
import {ActivatedRouteSnapshot, Router, RouterStateSnapshot} from '@angular/router';
import {forkJoin, Observable, Subscriber, Subscription} from 'rxjs';
import {catchError} from 'rxjs/operators';
import {CommonService} from '../../common/services/common.service';
import {DataStoreService} from './data-store.service';
import {
    CREATE_DISCLOSURE_ROUTE_URL,
    REPORTER_HOME_URL,
    HTTP_ERROR_STATUS,
    POST_CREATE_DISCLOSURE_ROUTE_URL
} from '../../app-constants';
import {NavigationService} from '../../common/services/navigation.service';
import {OpaService} from './opa.service';
import { InformationAndHelpTextService } from '../../common/services/informationAndHelpText.service';

@Injectable()
export class ResolveServiceService {

    private readonly _moduleCode = 'OPA23';

    $subscriptions: Subscription[] = [];

    constructor(
        private _commonService: CommonService,
        private _dataStore: DataStoreService,
        private _opaService: OpaService,
        private _router: Router,
        private _navigationService: NavigationService,
        private _informationAndHelpTextService : InformationAndHelpTextService
    ) {
    }

    canActivate(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot): Observable<boolean> {
        this._opaService.previousHomeUrl = this.setPreviousUrlPath(this._navigationService.navigationGuardUrl);
        return new Observable<boolean>((observer: Subscriber<boolean>) => {
            forkJoin(this.getHttpRequests(route)).subscribe((res: any[]) => {
                if (res.length > 1) {
                    this.updateSectionConfig(res[1]);
                }
                if (res[0]) {
                    this.updateProposalDataStore(res[0]);
                    // this.rerouteIfWrongPath(_state.url, res[0].coiDisclosure.reviewStatusCode, route, res[0].coiDisclosure.personId);
                    if (['3', '4', '7', '8', '5'].includes(res[0]?.reviewStatusType?.reviewStatusCode)) {
                        this.getCoiReview(res[0].opaDisclosureId, observer);
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

    rerouteIfWrongPath(currentPath: string, reviewStatusCode: string, route, personId: any) {
        let reRoutePath;
        if (['1', '5', '6'].includes(reviewStatusCode) && !currentPath
            .includes('create-disclosure') && personId == this._commonService.currentUserDetails.personId) {
            reRoutePath = CREATE_DISCLOSURE_ROUTE_URL;
        } else if (!['1', '5', '6'].includes(reviewStatusCode) && currentPath.includes('create-disclosure')) {
            reRoutePath = POST_CREATE_DISCLOSURE_ROUTE_URL;
        }
        if (reRoutePath) {
            this._router.navigate([reRoutePath], {queryParams: {disclosureId: route.queryParamMap.get('disclosureId')}});
        }
    }

    setPreviousUrlPath(previousUrl: string) {
        return previousUrl.includes('?') ? REPORTER_HOME_URL : previousUrl;
    }

    getCoiReview(disclosureId, observer) {
        this.$subscriptions.push(
            this._opaService.getOPAReview(disclosureId).subscribe((res: any) => {
                this._dataStore.updateStore(['opaReviewerList'], {opaReviewerList: res});
                this._opaService.isReviewActionCompleted = this._opaService.isAllReviewsCompleted(res);
                const reviewerDetail = this.getLoggedInReviewerInfo(res);
                if (reviewerDetail) {
                    this._opaService.isStartReview = reviewerDetail.reviewStatusTypeCode === '1' ? true : false;
                    this._opaService.isCompleteReview = reviewerDetail.reviewStatusTypeCode === '2' ? true : false;
                    this._opaService.isDisclosureReviewer = true;
                    this._opaService.$SelectedReviewerDetails.next(reviewerDetail);
                    this._opaService.currentOPAReviewForAction = reviewerDetail;
                }
                observer.next(true);
                observer.complete();
            }, _err => {
                observer.next(false);
                observer.complete();
                this._router.navigate([this._opaService.previousHomeUrl]);
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
            }));
    }

    getLoggedInReviewerInfo(coiReviewerList): any {
        const getReviewerDetail = coiReviewerList.find(item => item.assigneePersonId ===
            this._commonService.currentUserDetails.personId && item.reviewStatusTypeCode != '3');
        return getReviewerDetail;
    }

    private updateProposalDataStore(data: any) {
        this._dataStore.setStoreData({opaDisclosure: data});
    }

    private getHttpRequests(route: ActivatedRouteSnapshot): Observable<any>[] {
        const HTTP_REQUESTS = [];
        const MODULE_ID = route.queryParamMap.get('disclosureId');
        if (MODULE_ID) {
            HTTP_REQUESTS.push(this.loadDisclosure(MODULE_ID));
        }
        if (!this.isSectionConfigAlreadyFetched()) {
            HTTP_REQUESTS.push(this.getDisclosureSectionConfig());
        } else {
            this.setModuleConfiguration();
        }
        return HTTP_REQUESTS;
    }

    private loadDisclosure(disclosureId: string) {
        return this._opaService.loadOPA(disclosureId).pipe((catchError(error => this.redirectOnError(error))));
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

    isSectionConfigAlreadyFetched() {
        return Object.keys(this._dataStore.opaDisclosureSectionConfig).length;
    }

    getDisclosureSectionConfig() {
        return this._commonService.getDashboardActiveModules(this._moduleCode)
    }

    updateSectionConfig(sectionData: any): void {
        this._dataStore.opaDisclosureSectionConfig = this._commonService.getSectionCodeAsKeys(sectionData);
        this.setModuleConfiguration();
    }

    setModuleConfiguration() {
        this._informationAndHelpTextService.moduleConfiguration = this._dataStore.opaDisclosureSectionConfig;
    }

}
