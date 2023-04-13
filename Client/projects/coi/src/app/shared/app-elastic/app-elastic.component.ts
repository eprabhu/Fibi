/**
 * author : Mahesh Sreenath V M
 * a common elastic search component  currently only supports or condition for the Index fields
 * visit https://github.com/maheshpolus/elastic-search for updates and documents.
 * Visit https://developer.mozilla.org/en-US/docs/Web/API/Element/paste_event for more about paste event.
 */
import { Component, Input, Output, EventEmitter, OnChanges, OnInit, ChangeDetectorRef, ViewChild, ElementRef } from '@angular/core';
import { AppElasticService } from './app-elastic.service';

@Component({
	selector: 'app-elastic',
	templateUrl: './app-elastic.component.html',
	styleUrls: ['./app-elastic.component.css'],
	providers: [AppElasticService]
})
export class AppElasticComponent implements OnChanges, OnInit {

	@Input() options: any = {};
	@Input() placeHolder: string;
	@Input() clearField: String;
	@Input() isError: boolean;
	@Input() isDisabled: boolean;
  @Input() uniqueId: any;
	@Output() selectedResult: EventEmitter<any> = new EventEmitter<any>();
	@Output() onEmpty: EventEmitter<any> = new EventEmitter<any>();
	@ViewChild('searchField', { static: true }) searchField: ElementRef;
	searchText = '';
	isResultSelected = true;
	tempSearchText = '';
	timer: any;
	results = [];
	counter = -1;
	isActive = false;
	query = {
		query: { bool: { should: [] } },
		sort: [{ _score: { order: 'desc' } }],
		highlight: { pre_tags: ['<strong>'], post_tags: ['</strong>'] }
	};
	constructor(private _appElasticService: AppElasticService, private _ref: ChangeDetectorRef) { }

	ngOnInit() {
		this.searchText = this.options && this.options.defaultValue || '';
    this.setUnquieIdForSearchText();
	}

	ngOnChanges() {
		if (!this.isError) {
			this.searchText = this.options && this.options.defaultValue || '';
		}
		this.clearField = '' + this.clearField;
		if (this.clearField === 'true') {
			this.searchText = '';
			this.results = [];
		}
		this.isError ? this.searchField.nativeElement.classList.add('is-invalid')
			: this.searchField.nativeElement.classList.remove('is-invalid');
	}

	/**
	 * makes a elastic host connection and the result is formatted in string of label with bold tags
	 * for matching fields
	 */
	getElasticResult(): void {
		if (this.options) {
			clearTimeout(this.timer);
			this.timer = setTimeout(() => {
				this.isResultSelected = false;
				const temporaryText = this.searchText.trim();
				this.queryBuilder(temporaryText);
				const url = this.options.url + this.options.index + '/' + this.options.type + '/' + '_search?size=' + (this.options.size || 20);
				this._appElasticService.search(url, this.query).then((rst: any) => {
					this._ref.markForCheck();
					this.results = [];
					this.isActive = true;
					this.counter = -1;
					const src = ((rst.hits || {}).hits || []).map((hit) => hit._source);
					const hgt = ((rst.hits || {}).hits || []).map((hit) => hit.highlight);
					if (this.options.formatString) {
						let fieldsArray = [];
                        if (this.options.formatFields) {
                            fieldsArray = Object.keys(this.options.formatFields);
                        } else {
                            fieldsArray = Object.keys(this.options.fields);
                        }
						src.forEach((el: any, i: number) => {
							let lbl = this.options.formatString;
							fieldsArray.forEach(k => {
								lbl = lbl.replace(new RegExp(k, 'g'), this.getValueFromResponse(hgt[i][k], src[i][k], k));
							});
							lbl = lbl.replace(/null/g, '');
							lbl = el.external === 'Y' ? lbl + '<span class=\'badge badge-warning ml-3\'>External</span>' : lbl;
							this.results.push({ 'label': lbl, 'value': el });
						});
					} else {
						src.forEach((el: any, i: number) => {
							let lbl = '';
							Object.keys(this.options.fields).forEach(k => {
								lbl = ((hgt[i][k] || src[i][k]) != null) ? lbl + (hgt[i][k] || src[i][k]) + '|' : lbl + '';
							});
							lbl = lbl.slice(0, -1);
							lbl = el.external === 'Y' ? lbl + '<span class=\'badge badge-warning ml-3\'>External</span>' : lbl;
							this.results.push({ 'label': lbl, 'value': el });
						});
					}
					if (!this.results.length) {
						this.onEmpty.emit({ 'searchString': this.searchText });
						this.results.push({ 'label': 'No results' });
					}
				}, error => {
					this.results.push({ 'label': 'No results' });
				});
			}, this.options && this.options.debounceTime || 500);
		}
	}

	getValueFromResponse( highlight: string| number, source: string| number, value: string) {
		 const content = highlight || source;
		 if (content) {
			return this.checkForIcon(value) + content;
		 } else {
			return '';
		 }
	}

  setUnquieIdForSearchText() {
    this.searchField.nativeElement.id = this.uniqueId ?  this.uniqueId : Math.random() + '';
  }

	checkForIcon(k) {
		return this.options.icons && this.options.icons[k] ? this.options.icons[k] : '';
	}

	queryBuilder(searchText: string): void {
		this.query.highlight['fields'] = this.options.fields;
		let condition: any = {};
		this.query.query.bool = { should: [] };
		Object.keys(this.options.fields).forEach(field => {
			condition = Object.assign({}, condition);
			condition.match = {};
			condition.match[field] = { query: searchText.toLowerCase(), operator: 'or' };
			this.query.query.bool.should.push(condition);
		});
		if (this.options.extraConditions) {
			this.query.query.bool = {...this.query.query.bool, ...this.options.extraConditions};
		}
	}
	/**
	 * @param  {} value emit results on key enter mouse click to parent components
	 */
	emitSelectedObject(value: any): void {
		this.counter = -1;
		if (value) {
			this.selectedResult.emit(value);
			this.searchText = this.getSearchTextValue(value);
		} else {
			this.searchText = '';
			this.selectedResult.emit(null);
		}
		this.options.defaultValue = this.searchText;
		this.results = [];
		this.isActive = false;
	}

	getSearchTextValue(value): string {
		let lbl = this.options.contextField;
		Object.keys(value).forEach(k => { lbl = lbl.replace(new RegExp(k, 'g'), value[k]); });
		return lbl || this.searchText;
	}

	backSpaceEvent(): void {
		this.selectedResult.emit(null);
		this.getElasticResult();
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
		this.searchText = this.isResultSelected ? this.searchText : '';
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
		this.counter === -1 || this.counter === document.getElementsByClassName('search-result-item').length ?
			this.searchText = this.tempSearchText :
			this.searchText = this.results[this.counter].value[this.options.contextField];
	}
}
