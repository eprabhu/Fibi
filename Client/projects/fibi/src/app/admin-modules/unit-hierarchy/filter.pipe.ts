import { Pipe, PipeTransform } from '@angular/core';
@Pipe({
  name: 'filter'
})
export class FilterPipe implements PipeTransform {
    transform(items: any[], searchText: string): any[] {
        if (!items) {
            return [];
        }
        if (!searchText) {
            return [];
        }
        searchText = searchText.toLowerCase();
        return items.filter( v => {
            return v.unitNumber.toLowerCase().includes(searchText) || v.unitName.toLowerCase().includes(searchText);
        });
    }
}
