import { Pipe, PipeTransform } from '@angular/core';
import { getValueFromObject } from '../../../../../../fibi/src/app/common/utilities/custom-utilities';

@Pipe({
  name: 'SearchFilter'
})
export class SearchFilterPipe implements PipeTransform {

  transform(items: any[], searchText: string, keys: string[] = []): any[] {
    if (!items) {
        return [];
    }
    if (!searchText) {
        return items;
    }
    if (keys.length) {
        return items.filter(item => {
            return keys.reduce((acc, key) => acc + `'${(getValueFromObject(item, key) || '')}'`, '')
                .toLowerCase()
                .includes(searchText.trim().toLowerCase());
        });
    }
    return items.filter( L => {
        return Object.values(L).join().toLowerCase().includes(searchText.trim().toLowerCase());
    });
}

}
