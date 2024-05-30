/**
 * A common http search component works on the basis of search string and endpoint
 */
import { Component, Input, Output, EventEmitter, OnChanges, OnInit, ViewChild, ElementRef, OnDestroy, ChangeDetectorRef } from '@angular/core';
import { AppEndpointSearchService } from './app-endpoint-search.service';
import { Subscription } from 'rxjs';
import { replaceFormatStringWithValue } from '../../common/utilities/custom-utilities';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';

@Component({
    selector: 'app-endpoint-search',
    templateUrl: './app-endpoint-search.component.html',
    styleUrls: ['./app-endpoint-search.component.scss'],
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
    @Input() enableCommaSeparator = false;
    @Input() uniqueId = null;
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
    $subscriptions: Subscription[] = [];
    newSearchText = '';
    pipeSeparatedText: string = '';
	isShowResultValue = false;
	content = '';

    constructor(private _appEndpointSearchService: AppEndpointSearchService, private _ref: ChangeDetectorRef) { }

    ngOnInit() {
        this.searchText = this.httpOptions && this.httpOptions.defaultValue || '';
    }

    ngOnChanges() {
        if (!this.isError) {
            this.searchText = this.httpOptions && this.httpOptions.defaultValue || '';
        }
        setTimeout(() => {
            this.clearField = '' + this.clearField;
            if (this.clearField === 'true') {
                this.searchText = '';
                this.results = [];
                this.clearField = new String('false');
            }
        });
        this.isError ? this.searchField.nativeElement.classList.add('is-invalid')
            : this.searchField.nativeElement.classList.remove('is-invalid');
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    getCommaSeparatedText(temporaryText: string) {
        if (this.addSearchText && this.enableCommaSeparator && temporaryText.includes(',')) {
            let SEPARATED_TEXT_LIST = temporaryText.split(',').filter(value => value.trim() != '');
            SEPARATED_TEXT_LIST.forEach((value, index) => {
                if (index == 0) {
                    this.pipeSeparatedText = `"` + value.trim() + `" `;
                } else {
                    this.pipeSeparatedText += ` | "` + value.trim() + `"`;
                }
            });
        } else {
            this.pipeSeparatedText = '';
        }
    }

    /**
	 * calls an API with respect user inputs (path and search string) and the result is formatted in string of label
	 */
	getEndpointSearchResult(): void {
		if (this.httpOptions) {
      this.isShowResultValue = false;
			clearTimeout(this.timer);
			this.timer = setTimeout(() => {
				const temporaryText = this.searchText.trim();
        this.isShowResultValue = false;
				this.newSearchText = this.addSearchText ? temporaryText : '';
				this.$subscriptions.push(
					this._appEndpointSearchService.endpointSearch(this.httpOptions.path, temporaryText, this.httpOptions.params)
						.subscribe((resultArray: any) => {
							this.results = [];
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
                setTimeout(() => {
                  this.content = this.results.length + ' data found. Please use your arrow keys to navigate';
                  this.isShowResultValue = true;
                }, 1500);
							} else {
								this.onEmpty.emit({ 'searchString': this.searchText });
								if (!this.addSearchText) {
									this.results.push({ 'label': 'No results' });
								}
                setTimeout(() => {
                  this.content = 'No data found. Please search with another value';
                  this.isShowResultValue = true;
                }, 1500);
							}
						}));
			}, 500);
		}
	}
  setUnquieIdForSearchText() {
    this.searchField.nativeElement.id = this.uniqueId ?  this.uniqueId : Math.random() + '';
  }
    /**
     * call on focus this creates a empty search string call
     * use this wisely if data is large it can cause error
     */
    getEndpointSearchResultOnfocus(): void {
        if (this.searchOnFocus) {
            this.getEndpointSearchResult();
        } else {
            this.newSearchText = '';
            this.pipeSeparatedText = '';
        }
    }

    /**
     * @param  {} value emit results on key enter mouse click to parent components
     */
    emitSelectedObject(value: any): void {
        this.counter = -1;
        if (value) {
            this.onSelect.emit(value);
            setTimeout(() => {
                this.searchText = replaceFormatStringWithValue(this.httpOptions.contextField, value) || this.searchText;
            });
        } else {
            setTimeout(() => {
                this.searchText = '';
            });
            this.onSelect.emit(null);
        }
        setTimeout(() => {
            this.httpOptions.defaultValue = this.searchText;
        });
        this.results = [];
    }

    backSpaceEvent(): void {
        this.onSelect.emit(null);
        this.getEndpointSearchResult();
    }

    getEndPointSearchValueOnFocusOut(): void {
        this.searchValue.emit(this.searchText === 'ADD_NEW_SEARCH_TEXT' ? this.newSearchText : this.searchText);
        setTimeout(() => {
            this.searchText = this.isResultSelected ? this.searchText : '';
        });
        this.newSearchText = '';
        this.results = [];
    }

    emitSearchText(searchText, isPipeSeparated): void {
        if (this.enableCommaSeparator) {
            let selectedKeyword = [];
            if (isPipeSeparated) {
                if (searchText.includes('|')) {
                    searchText = searchText.replace(/"/g, '');
                }
                selectedKeyword = searchText.split('|').map(str => str.trim());;
                this.onNewValueSelect.emit(selectedKeyword);
            } else {
                selectedKeyword.push(searchText);
                this.onNewValueSelect.emit(selectedKeyword);
            }

        } else {
            this.onNewValueSelect.emit({ 'searchString': searchText });
        }
    }

    controlSearchEmit(event) {
        this.isResultSelected = true;
        if (event.option.id === 'ADD_NEW_SEARCH_TEXT') {
            this.emitSearchText(this.newSearchText, false)
        } else if (event.option.id === 'PIPE_SELECTED_VALUE') {
            this.emitSearchText(this.pipeSeparatedText, true)
        } else {
            this.emitSelectedObject(event.option.value ? event.option.value.value : null)
        }
        this.newSearchText = '';
    }

}