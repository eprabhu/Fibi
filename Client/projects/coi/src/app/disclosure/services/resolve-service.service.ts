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
    HOME_URL,
    POST_CREATE_DISCLOSURE_ROUTE_URL
} from '../../app-constants';
import {NavigationService} from '../../common/services/navigation.service';

@Injectable()
export class ResolveServiceService {

    $subscriptions: Subscription[] = [];
    constructor(
        private _commonService: CommonService,
        private _dataStore: DataStoreService,
        private _coiService: CoiService,
        private _router: Router,
        private _navigationService: NavigationService
    ) {
    }

    canActivate(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot): Observable<boolean> {
        this._coiService.previousHomeUrl = this.setPreviousUrlPath(this._navigationService.navigationGuardUrl);
        return new Observable<boolean>((observer: Subscriber<boolean>) => {
            forkJoin(this.getHttpRequests(route)).subscribe((res: any[]) => {
                if (res.length > 1) {
                    this.hideManualLoader();
                }
                if (res[0]) {
                    this.updateProposalDataStore(res[0]);
                    this.rerouteIfWrongPath(_state.url, res[0].coiDisclosure.reviewStatusCode, route, res[0].coiDisclosure.personId);
                    if (['3', '4', '7', '8'].includes(res[0].coiDisclosure.coiReviewStatusType.reviewStatusCode)) {
                        this.getCoiReview(res[0].coiDisclosure.disclosureId, observer);
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
        if (['1','5','6'].includes(reviewStatusCode) && !currentPath.includes('create-disclosure') && personId == this._commonService.currentUserDetails.personId ) {
            reRoutePath = CREATE_DISCLOSURE_ROUTE_URL;
        } else if (!['1','5','6'].includes(reviewStatusCode) && currentPath.includes('create-disclosure')) {
            reRoutePath = POST_CREATE_DISCLOSURE_ROUTE_URL;
        }
        if (reRoutePath) {
            this._router.navigate([reRoutePath], {queryParams: {disclosureId: route.queryParamMap.get('disclosureId')}});
        }
    }

    setPreviousUrlPath(previousUrl: string) {
        return previousUrl.includes('?') ? HOME_URL : previousUrl;
    }

    private updateProposalDataStore(data: any) {
        this._dataStore.setStoreData(data);
    }

    private getHttpRequests(route: ActivatedRouteSnapshot): Observable<any>[] {
        const HTTP_REQUESTS = [];
        const MODULE_ID = route.queryParamMap.get('disclosureId');
        if (MODULE_ID) { HTTP_REQUESTS.push(this.loadDisclosure(MODULE_ID)); }
        return HTTP_REQUESTS;
    }

    private loadDisclosure(disclosureId: string) {
        return this._coiService.loadDisclosure(disclosureId).pipe((catchError(error => this.redirectOnError(error))));
    }

    private hideManualLoader() {
        this._commonService.isShowLoader.next(false);
        this._commonService.isManualLoaderOn = false;
    }

    private redirectOnError(error) {
            this._commonService.showToast(HTTP_ERROR_STATUS, (error.error) ?
                error.error : 'Something went wrong. Please try again.');
        if (error.status === 403 && error.error !== 'DISCLOSURE_EXISTS') {
            this._commonService.forbiddenModule = '8';
            this._router.navigate(['/coi/error-handler/403']);
            return new Observable(null);
        } else {
            this._router.navigate([HOME_URL]);
            // this._commonService.showToast(HTTP_ERROR_STATUS,
            //     error.error !== 'DISCLOSURE_EXISTS' ? 'Please try again later.' : 'Disclosure already exists.');
            return new Observable(null);
        }
    }

    getCoiReview(disclosureId, observer) {
        this.$subscriptions.push(
            this._coiService.getCoiReview(disclosureId).subscribe((res: any) => {
                this._dataStore.updateStore(['coiReviewerList'], { coiReviewerList: res });
                this._coiService.isReviewActionCompleted = this._coiService.isAllReviewsCompleted(res);
                const reviewerDetail = this.getLoggedInReviewerInfo(res);
                if (reviewerDetail) {
                    this._coiService.isStartReview = reviewerDetail.reviewStatusTypeCode === '1' ? true : false;
                    this._coiService.isCompleteReview = reviewerDetail.reviewStatusTypeCode === '3' ? true : false;
                    this._coiService.isDisclosureReviewer = true;
                    this._coiService.$SelectedReviewerDetails.next(reviewerDetail);
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
            this._commonService.currentUserDetails.personId);
        return getReviewerDetail;
    }

}
