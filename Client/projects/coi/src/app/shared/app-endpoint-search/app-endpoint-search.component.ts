/**
 * A common http search component works on the basis of search string and endpoint
 */
import { Component, Input, Output, EventEmitter, OnChanges, OnInit, ViewChild, ElementRef, OnDestroy, ChangeDetectorRef } from '@angular/core';
import { AppEndpointSearchService } from './app-endpoint-search.service';
import { Subscription } from 'rxjs';
import {subscriptionHandler} from "../../../../../fibi/src/app/common/utilities/subscription-handler";
import {replaceFormatStringWithValue} from "../../../../../fibi/src/app/common/utilities/custom-utilities";

@Component({
	selector: 'app-endpoint-search',
	templateUrl: './app-endpoint-search.component.html',
	styleUrls: ['./app-endpoint-search.component.css'],
	providers: [AppEndpointSearchService]
})
export class AppEndpointSearchComponent implements OnChanges, OnInit, OnDestroy {

	@Input() httpOptions: any = {};
	@Input() placeHolder: string;
	@Input() searchOnFocus = false;
	@Input() addSearchText = false;
	@Input() clearField: String;
	@Input() isError: boolean;
	@Input() isDisabled: boolean;
	@Output() onSelect: EventEmitter<any> = new EventEmitter<any>();
	@Output() onEmpty: EventEmitter<any> = new EventEmitter<any>();
	@Output() onNewValueSelect: EventEmitter<any> = new EventEmitter<any>();
	@Output() searchValue: EventEmitter<any> = new EventEmitter<any>();
	@ViewChild('searchField', { static: true }) searchField: ElementRef;
	@ViewChild('confirmProceedBtn', { static: true }) confirmProceedBtn: ElementRef;

	searchText = '';
	tempSearchText = '';
	isResultSelected = true;
	timer: any;
	results = [];
	counter = -1;
	isActive = false;
	$subscriptions: Subscription[] = [];
	newSearchText = '';
	constructor(private _appEndpointSearchService: AppEndpointSearchService, private _ref: ChangeDetectorRef) { }

	ngOnInit() {
		this.searchText = this.httpOptions && this.httpOptions.defaultValue || '';
	}
	ngOnChanges() {
		if (!this.isError) {
			this.searchText = this.httpOptions && this.httpOptions.defaultValue || '';
		}
		this.clearField = '' + this.clearField;
		if (this.clearField === 'true') {
			this.searchText = '';
			this.results = [];
		}
		this.isError ? this.searchField.nativeElement.classList.add('is-invalid')
			: this.searchField.nativeElement.classList.remove('is-invalid');
	}

	ngOnDestroy() {
		subscriptionHandler(this.$subscriptions);
	}
	/**
	 * calls an API with respect user inputs (path and search string) and the result is formatted in string of label
	 */
	getEndpointSearchResult(): void {
		if (this.httpOptions) {
			clearTimeout(this.timer);
			this.timer = setTimeout(() => {
				const temporaryText = this.searchText.trim();
				this.newSearchText = this.addSearchText ? temporaryText : '';
				this.$subscriptions.push(
					this._appEndpointSearchService.endpointSearch(this.httpOptions.path, temporaryText, this.httpOptions.params)
						.subscribe((resultArray: any) => {
							this.results = [];
							this.isActive = true;
							this._ref.markForCheck();
							this.isResultSelected = this.httpOptions.defaultValue === this.searchText ? true : false;
							this.counter = -1;
							if (resultArray.length > 0) {
								if (this.httpOptions.formatString) {
									resultArray.forEach((el, i) => {
										const label = replaceFormatStringWithValue(this.httpOptions.formatString, resultArray[i]);
										this.results.push({ 'label': label, 'value': el });
									});
								}
							} else {
								this.onEmpty.emit({ 'searchString': this.searchText });
								if (!this.addSearchText) {
									this.results.push({ 'label': 'No results' });
								}
							}
						}));
			}, 500);
		}
	}
	/**
	 * call on focus this creates a empty search string call
	 * use this wisely if data is large it can cause error
	 */
	getEndpointSearchResultOnfocus(): void {
		if (this.searchOnFocus) {
			this.getEndpointSearchResult();
		}
	}

	/**
	 * @param  {} value emit results on key enter mouse click to parent components
	 * @param label formatted string
	 */
	emitSelectedObject(value: any): void {
		this.counter = -1;
		if (value) {
			this.onSelect.emit(value);
			this.searchText = replaceFormatStringWithValue(this.httpOptions.contextField, value) || this.searchText;
		} else {
			this.searchText = '';
			this.onSelect.emit(null);
		}
		this.httpOptions.defaultValue = this.searchText;
		this.results = [];
		this.isActive = false;
	}

	backSpaceEvent(): void {
		this.onSelect.emit(null);
		this.getEndpointSearchResult();
	}
	/**
	 * @param  {} event used to update counter value for keyboard event listener
	 */
	upArrowEvent(event: Event): void {
		event.preventDefault();
		this.removeHighlight();
		this.counter >= 0 ? this.counter-- : this.counter = document.getElementsByClassName('search-result-item').length - 1;
		this.addHighlight();
		this.updateSearchField();
	}
	/**
	 * @param  {} event  used to update counter value for keyboard event listener and adds a highlight class
	 */
	downArrowEvent(event: Event): void {
		event.preventDefault();
		this.removeHighlight();
		this.counter < document.getElementsByClassName('search-result-item').length - 1 ? this.counter++ : this.counter = -1;
		this.addHighlight();
		this.updateSearchField();
	}
	/**
	 * @param  {} event
	 *  handles the click outside the result box updates counter and clear results
	 */
	hideSearchResults(): void {
		this.isActive = false;
		this.results = [];
		this.counter = -1;
	}
	/** listens for enter key event . triggers the click on selected li
	 */
	enterKeyEvent(): void {
		if (this.counter > -1) {
			this.isResultSelected = true;
			(document.getElementsByClassName('search-result-item')[this.counter] as HTMLInputElement).click();
			(document.activeElement as HTMLInputElement).blur();
			this.hideSearchResults();
		}
	}
	/**
	 * removes the highlight from the previous li node if true
	 * updates the temp search value with user typed value for future reference
	 */
	removeHighlight(): void {
		const el = (document.getElementsByClassName('search-result-item')[this.counter] as HTMLInputElement);
		if (el) {
			el.classList.remove('highlight');
		} else {
			this.tempSearchText = this.searchText;
		}
	}
	/**
	 * updates the li with 'highlight' class
	 */
	addHighlight(): void {
		const el = (document.getElementsByClassName('search-result-item')[this.counter] as HTMLInputElement);
		if (el) {
			el.scrollIntoView({ block: 'nearest' });
			el.classList.add('highlight');
		}
	}
	/**
	 * updates the search field with temp value once user reaches the bottom or top of the list
	 */
	updateSearchField(): void {
		if (document.getElementById('add-field') &&
			(document.getElementById('add-field') as HTMLInputElement).classList.contains('highlight')) {
			this.searchText = this.newSearchText;
		} else {
			this.counter === -1 || this.counter === document.getElementsByClassName('search-result-item').length ?
				this.searchText = this.tempSearchText :
				this.searchText = this.results[this.addSearchText ? this.counter - 1 : this.counter].value[this.httpOptions.contextField];
		}
	}

	getEndPointSearchValueOnFocusOut(event): void {
		this.searchValue.emit(event);
		this.searchText = this.isResultSelected ? this.searchText : '';
		this.isActive = false;
	}

	emitSearchText(): void {
		this.onNewValueSelect.emit({ 'searchString': this.searchText });
		this.isActive = false;
	}

	// checkDuplication(searchText) {
	//   const searchResult = this.results.find((list) => list.label === searchText);
	//   if (searchResult) {
	//     (this.confirmProceedBtn.nativeElement as HTMLInputElement).click();
	//   } else {
	//     this.emitSearchText();
	//   }
	// }

	// setFocusOnSearchField() {
	//   (this.searchField.nativeElement as HTMLInputElement).focus();
	// }
}

