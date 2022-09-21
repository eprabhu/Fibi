import { CommonService } from './../../../../common/services/common.service';
import { Component, OnInit, Input, OnChanges, ChangeDetectionStrategy, ChangeDetectorRef } from '@angular/core';
import { forkJoin ,  of ,  Observable ,  SubscriptionLike as ISubscription ,  Subject } from 'rxjs';
import { OverviewService } from './overview.service';
import { CompareDetails, CompareData } from '../../interfaces';
import { switchMap } from 'rxjs/operators';
import { CommonDataService } from '../../../services/common-data.service';
/**
 * Developed by Mahesh Sreenath V M / Aravind P S
 * Acts as a independent component that fetches the data for a given awardId.
 * The required details is fetched as input from parent.
 * The comparisonDetails have the details for fetching the required data.
 */

@Component({
  selector: 'app-overview',
  templateUrl: './overview.component.html',
  styleUrls: ['./overview.component.css'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [OverviewService]
})

export class OverviewComponent implements OnChanges {

  constructor(private _overviewService: OverviewService,
              private _CDRef: ChangeDetectorRef,
              public _commonData: CommonService,
              public _commonDataService: CommonDataService) { }

  @Input() comparisonDetails: CompareDetails;
  @Input() currentMethod: String;
  currentMethodChild: String;
  overViewDataCache = {};
  comparisonData: CompareData = {
    base: {},
    current: {},
    awardId: '',
    awardNumber: '',
    sequenceNumber : null,
    moduleVariableSections: [],
    currentSequenceNumber: null,
    isActiveComparison: false
  };
  @Input() helpText: any = {};
  /**
   * On changes from parent the currentMethod will be updated here AT the first time on application load
   * there will be no award number available to fetch data. to avoid empty service call
   * baseAwardId check is added the currentMethod will be a String constructor to convert it into
   * a string we use + operator.See the doc below for technical clarification
   * https://docs.google.com/document/d/1vAACL0gDF2_tMW46ilpEsbZ0qRAFECEnPT4x4QoW2Mg/edit?usp=sharing
   */
  ngOnChanges() {
    if (this.comparisonDetails.baseAwardId) {
      this.currentMethod + '' === 'COMPARE' ? this.compareOverView() : this.setCurrentView();
    }
  }

  /**
   * @returns void
   * compare the data actually it fetches the data for comparison.
   * Since wee need two different award version data to compare. forkJoin is used so that
   * we trigger the compare function once both version data has been loaded.
   * This also updates the data to the cache so that the next time we can
   * reuse the same data instead of making a DB call. improves performance
   */
  compareOverView(): void {
    forkJoin(this.getAwardDetails('base'), this.getAwardDetails('current')).subscribe(
      data => {
        this.updateCache(data[0], 'base');
        this.updateCache(data[1], 'current');
        this.updateChildComponents(data[0], data[1]);
        this.currentMethodChild = new String('COMPARE');
        this._CDRef.detectChanges();
    });
  }
  /**
   * @returns void
   * sets the value to view baseAwardId is used since base is always compared to current.
   * This also updates the data to the cache so that the next time we can
   * reuse the same data instead of making a DB call. improves performance
   */
  setCurrentView(): void {
    this.getAwardDetails('base').subscribe(data => {
      this.updateCache(data, 'base');
      this.comparisonData.base = data;
      this.updateChildComponents(data, {});
      this.currentMethodChild = new String('VIEW');
      this._CDRef.detectChanges();
    });
  }

  /**
   * @param  {string} type
   * @returns Observable
   * fetches the data from server if its not available in cache. only return the Observable.
   * Subscription will be done at the function which invokes this method.
   */
  getAwardDetails(type: string): Observable<any> {
    const AWARD_ID = this.getAwardId(type);
    if (this.checkInCache(AWARD_ID)) {
      return of(this.deepCopy(this.overViewDataCache[AWARD_ID]));
    } else {
      const request = this.getRequestDetails(AWARD_ID);
      return this._overviewService.getAwardGeneralData(request);
    }
  }
  /**
   * @param  {string} type
   * @returns string
   * return the award id from the input Comparison details according to the Type.
   * if base is the type reruns baseAwardId other wise currentAwardId.
   */
  getAwardId(type: string): string {
    return type === 'base' ? this.comparisonDetails.baseAwardId : this.comparisonDetails.currentAwardId;
  }

  getRequestDetails(awardId: string): Object {
    const REQUEST: any = {};
    REQUEST.awardId = awardId;
    return REQUEST;
  }

  updateChildComponents(base: object, current: object) {
    this.comparisonData.base = base;
    this.comparisonData.current = current;
    this.comparisonData.awardId = this.comparisonDetails.baseAwardId;
    this.comparisonData.awardNumber = this.comparisonDetails.awardNumber;
    this.comparisonData.sequenceNumber = this.comparisonDetails.sequenceNumber;
    this.comparisonData.moduleVariableSections = this.comparisonDetails.moduleVariableSections;
    this.comparisonData.currentSequenceNumber = this.comparisonDetails.currentSequenceNumber;
    this.comparisonData.isActiveComparison = this.comparisonDetails.isActiveComparison;
  }

  updateCache(data: any, type: string): void {
    const awardId = this.getAwardId(type);
    if (!this.checkInCache(awardId)) {
      this.overViewDataCache[awardId] = this.deepCopy(data);
    }
  }

  checkInCache(cacheName: string) {
    return !!Object.keys(this.overViewDataCache).find(key => key === cacheName);
  }

  deepCopy(data: any): any {
    return JSON.parse(JSON.stringify(data));
  }

}
