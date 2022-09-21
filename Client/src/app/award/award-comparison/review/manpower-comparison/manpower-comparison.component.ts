import { ChangeDetectionStrategy, ChangeDetectorRef, Component, Input, OnChanges, OnDestroy, OnInit } from '@angular/core';
import { forkJoin, Observable, of, Subscription } from 'rxjs';
import { CommonService } from '../../../../common/services/common.service';
import { CurrencyParserService } from '../../../../common/services/currency-parser.service';
import { DateParserService } from '../../../../common/services/date-parser.service';
import { compareArray } from '../../../../common/utilities/array-compare';
import { subscriptionHandler } from '../../../../common/utilities/subscription-handler';
import { calculateUncommittedAmount } from '../../../man-power/manpower-utilities';
import { CommonDataService } from '../../../services/common-data.service';
import { AwardManpower, AwardManpowerResource } from '../../comparison-constants';
import { CompareDetails } from '../../interfaces';
import { ToolkitEventInteractionService } from '../../toolkit-event-interaction.service';
import { ManpowerComparisonService } from './manpower-comparison.service';

@Component({
  selector: 'app-manpower',
  templateUrl: './manpower-comparison.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
  styleUrls: ['./manpower-comparison.component.css'],
  providers: [ManpowerComparisonService]
})
export class ManpowerComponent implements OnInit, OnChanges, OnDestroy {

  @Input() comparisonDetails: CompareDetails;
  @Input() currentMethod: string;
  isManpowerWidgetOpen = true;
  manpowerOther: any = [];
  manpowerEOM: any = [];
  manpowerRSS: any = [];
  manpowerCache = {};
  $subscriptions: Subscription[] = [];
  currentDetails: any;
  canViewStaff = false;
  canViewStudent = false;
  canViewOthers = false;
  isShowEOMAllDetails: any = [];
  isShowRSSAllDetails: any = [];

  constructor(private _manpowerService: ManpowerComparisonService, public _commonService: CommonService,
    private _CDRef: ChangeDetectorRef, private _commonData: CommonDataService,
    public dateFormatter: DateParserService, public currencyFormatter: CurrencyParserService,
    private _toolKitEvents: ToolkitEventInteractionService) { }

  ngOnInit() {
    this.getPermissions();
  }

  ngOnChanges() {
    if (this.comparisonDetails.baseAwardId) {
      this.currentMethod + '' === 'COMPARE'
        && (this._toolKitEvents.checkSectionTypeCode('131', this.comparisonDetails.moduleVariableSections)
          || this.comparisonDetails.isActiveComparison ) ? this.compareManpower() : this.setCurrentView();
    }
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  getPermissions(): void {
    this.canViewStaff = this._commonData.checkDepartmentLevelRightsInArray('MANPOWER_VIEW_STAFF_PLAN') ||
      this._commonData.checkDepartmentLevelRightsInArray('MANPOWER_MAINTAIN_STAFF');
    this.canViewStudent = this._commonData.checkDepartmentLevelRightsInArray('MANPOWER_VIEW_STUDENT_PLAN') ||
      this._commonData.checkDepartmentLevelRightsInArray('MANPOWER_MAINTAIN_STUDENT');
    this.canViewOthers = this._commonData.checkDepartmentLevelRightsInArray('MANPOWER_VIEW_OTHERS_PLAN') ||
      this._commonData.checkDepartmentLevelRightsInArray('MANPOWER_MAINTAIN_OTHERS');
  }
  /**
   * @returns void
   * compare the data actually it fetches the data for comparison.
   * Since wee need two different award version data to compare. forkJoin is used so that
   * we trigger the compare function once both version data has been loaded.
   * This also updates the data to the cache so that the next time we can
   * reuse the same data instead of making a DB call. improves performance
   */
  compareManpower(): void {
    this.$subscriptions.push(forkJoin(this.fetchManpower('base'), this.fetchManpower('current')).subscribe(
      data => {
        if (data[0].manpowerCategory.awardManpowerDetails.Staff && this.canViewStaff) {
          this.manpowerBalanceDetails(data[0].manpowerCategory.awardManpowerDetails.Staff, true);
        }
        if (data[1].manpowerCategory.awardManpowerDetails.Staff && this.canViewStaff) {
          this.manpowerBalanceDetails(data[1].manpowerCategory.awardManpowerDetails.Staff, true);
        }
        if (data[0].manpowerCategory.awardManpowerDetails.Student && this.canViewStudent) {
          this.manpowerBalanceDetails(data[0].manpowerCategory.awardManpowerDetails.Student, false);
        }
        if (data[1].manpowerCategory.awardManpowerDetails.Student && this.canViewStudent) {
          this.manpowerBalanceDetails(data[1].manpowerCategory.awardManpowerDetails.Student, false);
        }
        this.updateCache(data[0], 'base');
        this.updateCache(data[1], 'current');
        if (data[0].accountNumber && data[1].accountNumber) {
          this.manpowerEOM = this.compareCategory(this.canViewStaff, data[0], data[1], 'Staff');
          this.manpowerRSS = this.compareCategory(this.canViewStudent, data[0], data[1], 'Student');
          if (this.canViewOthers) {
            this.manpowerOther = this.compareManpowerArray(data[0], data[1], 'Others');
          }
        }
        this.updateCurrentMethod('COMPARE');
        this._CDRef.detectChanges();
      }));
  }

  compareCategory(canView: boolean, base: any, variant: any, sectionName: string): any[] {
    return canView ? this.compareManpowerArray(base, variant, sectionName) : [];
  }
  /**
   * @returns void
   * sets the value to view baseAwardId is used since base is always compared to current.
   * This also updates the data to the cache so that the next time we can
   * reuse the same data instead of making a DB call. improves performance
   */
  setCurrentView(): void {
    this.$subscriptions.push(this.fetchManpower('base').subscribe((data: any) => {
      if (data.manpowerCategory.awardManpowerDetails.Staff && this.canViewStaff) {
        this.manpowerBalanceDetails(data.manpowerCategory.awardManpowerDetails.Staff, true);
      }
      if (data.manpowerCategory.awardManpowerDetails.Student && this.canViewStudent) {
        this.manpowerBalanceDetails(data.manpowerCategory.awardManpowerDetails.Student, false);
      }
      this.updateCache(data, 'base');
      this.manpowerEOM = data.manpowerCategory.awardManpowerDetails.Staff;
      this.manpowerRSS = data.manpowerCategory.awardManpowerDetails.Student;
      this.manpowerOther = data.manpowerCategory.awardManpowerDetails.Others;
      this.updateCurrentMethod('VIEW');
      this._CDRef.detectChanges();
    }));
  }
  /**
   * @param  {} details
   * for setting manpower staff details like committedBalance and uncommittedAmount which are not stored in db
   */
  manpowerBalanceDetails(details: any, calculationFlag: boolean): void {
    details.map(element => {
      if (calculationFlag) {
        element.committedBalance = element.sapCommittedAmount - element.expenseAmount;
        element.uncommittedAmount = calculateUncommittedAmount(element.awardManpowerResource,
          element.budgetAmount, element.sapCommittedAmount);
      } else {
        element.balance = element.budgetAmount - element.sapCommittedAmount;
      }
    });
  }
  /**
   * @param  {any} base
   * @param  {any} current
   * @param  {string} sectionName : defines the section name which has to be compared
   * compared the array of category elements and also the array of resources in each category
   */
  compareManpowerArray(base: any, current: any, sectionName: string): any {
    this.currentDetails = JSON.parse(JSON.stringify(this.getCurrentSection(current, sectionName)));
    const compareManpower = compareArray(base.manpowerCategory.awardManpowerDetails[sectionName],
      current.manpowerCategory.awardManpowerDetails[sectionName], AwardManpower.reviewSectionUniqueFields,
      AwardManpower.reviewSectionSubFields);
    compareManpower.forEach(element => {
      if (element.status === 0) {
        const currentCategory = this.findInCurrentCategory(element.budgetReferenceNumber);
        element.awardManpowerResource = compareArray(element[AwardManpowerResource.reviewSectionName],
          currentCategory[AwardManpowerResource.reviewSectionName], AwardManpowerResource.reviewSectionUniqueFields,
          AwardManpowerResource.reviewSectionSubFields);
      }
    });
    return compareManpower;
  }

  getCurrentSection(current: any, sectionName: string): any[] {
    return current.manpowerCategory.awardManpowerDetails[sectionName] ?
      current.manpowerCategory.awardManpowerDetails[sectionName] : [];
  }
  /**
   * @param  {string} type
   * @returns Observable
   * fetches the data from server if its not available in cache. only return the Observable.
   * Subscription will be done at the function which invokes this method.
   */
  fetchManpower(type: string): Observable<any> {
    const AWARD_ID = this.getAwardId(type);
    if (this.checkInCache(AWARD_ID)) {
      return of(this.deepCopy(this.manpowerCache[AWARD_ID]));
    } else {
      return this._manpowerService.fetchManpowerDetails(AWARD_ID);
    }
  }

  findInCurrentCategory(budgetReferenceNumber: string) {
    return this.currentDetails.find(current => current.budgetReferenceNumber === budgetReferenceNumber);
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
  checkInCache(cacheName: string) {
    return !!Object.keys(this.manpowerCache).find(key => key === cacheName);
  }

  deepCopy(data: any): any {
    return JSON.parse(JSON.stringify(data));
  }

  updateCache(data: any, type: string): void {
    const awardId = this.getAwardId(type);
    if (!this.checkInCache(awardId)) {
      this.manpowerCache[awardId] = this.deepCopy(data);
    }
  }

  updateCurrentMethod(method: string): void {
    this.currentMethod = method;
  }

}
