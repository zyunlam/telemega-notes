/*
 * Copyright © 2019 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef _AO_DMA_SAM21_H_
#define _AO_DMA_SAM21_H_

void
ao_dma_init(void);

void
_ao_dma_start_transfer(uint8_t		id,
		       const void	*src,
		       void		*dst,
		       uint16_t		count,
		       uint32_t		chctrlb,
		       uint16_t		btctrl,
		       void		(*callback)(uint8_t id, void *closure),
		       void		*closure);

void
_ao_dma_done_transfer(uint8_t id);

void
ao_dma_dump(char *where);

/*
 * DMA is only used for SERCOM
 */

#define AO_SERCOM_DMA_BASE			0U
#define AO_SERCOM_INPUT_DMA_ID(id)		((uint8_t) ((id) * 2U + 0U + AO_SERCOM_DMA_BASE))
#define AO_SERCOM_OUTPUT_DMA_ID(id)		((uint8_t) ((id) * 2U + 1U + AO_SERCOM_DMA_BASE))

#endif /* _AO_DMA_SAM21_H_ */
