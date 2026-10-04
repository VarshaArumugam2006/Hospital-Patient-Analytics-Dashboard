from PIL import Image, ImageDraw, ImageFont
import math

size = (256, 256)
img = Image.new("RGBA", size, (0, 0, 0, 0))
draw = ImageDraw.Draw(img)

# Rounded rectangle background with gradient
for y in range(size[1]):
    ratio = y / size[1]
    r = int(11 * (1 - ratio) + 30 * ratio)
    g = int(19 * (1 - ratio) + 58 * ratio)
    b = int(43 * (1 - ratio) + 138 * ratio)
    draw.line([(16, y), (240, y)], fill=(r, g, b, 255))

# Mask to rounded rectangle
mask = Image.new("L", size, 0)
mask_draw = ImageDraw.Draw(mask)
mask_draw.rounded_rectangle([16, 16, 240, 240], radius=48, fill=255)
img.putalpha(mask)

# Re-draw on RGBA to overlay graphics
overlay = Image.new("RGBA", size, (0, 0, 0, 0))
o_draw = ImageDraw.Draw(overlay)

# Medical Cross
cross_coords = [
    (104, 45), (152, 45), (152, 104), (211, 104),
    (211, 152), (152, 152), (152, 211), (104, 211),
    (104, 152), (45, 152), (45, 104), (104, 104)
]
o_draw.polygon(cross_coords, fill=(14, 165, 233, 210))

# Pulse ECG line
pts = [
    (30, 128), (70, 128), (90, 155), (115, 65),
    (140, 185), (165, 105), (185, 128), (226, 128)
]
for i in range(len(pts) - 1):
    o_draw.line([pts[i], pts[i+1]], fill=(52, 211, 153, 255), width=7)
    o_draw.line([pts[i], pts[i+1]], fill=(255, 255, 255, 255), width=3)

# Glowing dots on peaks
o_draw.ellipse([110, 60, 120, 70], fill=(255, 255, 255, 255))
o_draw.ellipse([135, 180, 145, 190], fill=(52, 211, 153, 255))

# Outer border
o_draw.rounded_rectangle([16, 16, 240, 240], radius=48, outline=(56, 189, 248, 120), width=4)

final = Image.alpha_composite(img, overlay)
final.save("www/logo.png", "PNG")
print("Saved www/logo.png")
